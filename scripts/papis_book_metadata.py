#!/usr/bin/env -S uv run --script

# /// script
# dependencies = [
#   "ebooklib",
#   "pyyaml",
# ]
# ///
"""Merge the embedded metadata of an EPUB or PDF with `info.yaml'."""

from ebooklib import epub

import json
import posixpath
import re
import shutil
import subprocess
import sys
import zipfile
from pathlib import Path
from urllib.parse import unquote
from xml.sax.saxutils import escape

import yaml

UNSAFE = re.compile(r'[/\\*?"<>|\x00-\x1f]')
COLON = re.compile(r"\s*:\s*")

# info.yaml key -> OPF/DC element name for reading/metadata merging
DC_TAGS = {"title": "title", "date": "date"}

# Additional OPF/DC tags written from structured data
DC_WRITE_TAGS = {"title": "title", "author": "creator", "date": "date"}

# info.yaml key -> exiftool tag written for PDF files
PDF_WRITE_TAGS = {"title": "Title", "author": "Author", "date": "CreateDate"}

# exiftool date: "2026:09:23 12:55:55+08:00" (subseconds optional)
EXIF_DATE = re.compile(
    r"(\d{4}):(\d{2}):(\d{2})[ T](\d{2}):(\d{2}):(\d{2})"
    r"(\.\d+)?(Z|[+-]\d{2}:?\d{2})?"
)

# date we can express in a PDF: it needs at least a full day
ISO_DATE = re.compile(
    r"(\d{4})-(\d{2})-(\d{2})"
    r"(?:[T ](\d{2}):(\d{2})(?::(\d{2}))?(\.\d+)?(Z|[+-]\d{2}:?\d{2})?)?"
)


def meta(book, name):
    """Return the first non-empty DC metadata value, or None."""
    values = book.get_metadata("DC", name)
    for value, _attrs in values:
        if value and str(value).strip():
            return str(value).strip()
    return None


def parse_author_name(name):
    """Return {'family': ..., 'given': ...} for one author NAME."""
    name = (name or "").strip()
    if not name:
        return None
    if "," in name:
        family, given = name.split(",", 1)
        return {"family": family.strip(), "given": given.strip()}
    parts = name.split()
    if len(parts) == 1:
        return {"family": parts[0], "given": ""}
    return {"family": parts[-1], "given": " ".join(parts[:-1])}


def author_list_from_text(text):
    """Split 'A; B' or 'A and B' into structured author entries."""
    if not text:
        return []
    authors = []
    for piece in re.split(r"\s*;\s*|\s+and\s+", str(text)):
        author = parse_author_name(piece)
        if author:
            authors.append(author)
    return authors


def author_list_to_text(authors):
    """Format structured authors for EPUB/PDF metadata."""
    names = []
    for author in authors or []:
        family = str(author.get("family", "")).strip()
        given = str(author.get("given", "")).strip()
        name = " ".join(part for part in (given, family) if part)
        if name:
            names.append(name)
    return " and ".join(names)


def primary_author(authors):
    """Return the display name of the first structured author."""
    if not authors:
        return None
    return author_list_to_text([authors[0]]) or None


def year_of(date):
    """'2026-09-23T12:55:55Z' / '2026-09' / '2026' -> '2026'."""
    match = re.search(r"\d{4}", str(date or ""))
    return match.group(0) if match else None


def as_text(value):
    """yaml turns `year: 2026` into int; treat scalars as text."""
    if isinstance(value, bool) or value is None:
        return None
    if isinstance(value, (int, float)):
        return str(int(value)) if float(value).is_integer() else str(value)
    if isinstance(value, str) and value.strip():
        return value.strip()
    return None


def iso_date(value):
    """exiftool's '2026:09:23 12:55:55+08:00' -> ISO 8601 for info.yaml."""
    match = EXIF_DATE.fullmatch(str(value or "").strip())
    if not match:
        return None
    year, month, day, hour, minute, second, subsec, zone = match.groups()
    return (
        f"{year}-{month}-{day}T{hour}:{minute}:{second}"
        f"{subsec or ''}{zone or ''}"
    )


def pdf_date(value):
    """Convert an info.yaml date to the format exiftool writes into a PDF.

    A PDF creation date always carries a time, so a plain day such as
    '2026-09-23' becomes midnight.  Return None for a year or
    year-month, which cannot be expressed without inventing a day.
    """
    match = ISO_DATE.fullmatch(str(value or "").strip())
    if not match:
        return None
    year, month, day, hour, minute, second, subsec, zone = match.groups()
    return (
        f"{year}:{month}:{day} {hour or '00'}:{minute or '00'}:{second or '00'}"
        f"{subsec or ''}{zone or ''}"
    )


def reconcile_date(date, year, file_date=None):
    """Make (date, year) agree, honouring whichever field was edited.

    `date` may be a full date, a year-month or a year only, e.g.
    '2026-09-23T12:55:55Z', '2026-09' or '2026'. `year` is normally the
    bare 4-digit value stored in info.yaml. When the two disagree, the
    field the user changed relative to the embedded metadata wins; if
    both changed, `date` wins (and a warning is printed).
    """
    date = as_text(date)
    year = as_text(year)
    if not year or not re.fullmatch(r"\d{4}", year):
        year = None

    date_year = year_of(date)
    if date and year and date_year != year:
        date_changed = date != file_date
        year_changed = year != year_of(file_date)
        if year_changed and not date_changed:
            # only `year` was edited -> rewrite the year inside `date`
            date = re.sub(r"\d{4}", year, date, count=1)
            date_year = year
        else:
            if date_changed and year_changed:
                print(
                    f"warning: date {date!r} and year {year!r} disagree, "
                    "keeping date"
                )
            year = date_year

    if date and not year:
        year = date_year
    if year and not date:
        date = year
    return date, year


def safe_name(text):
    return UNSAFE.sub("", COLON.sub(" - ", text)).strip().strip(".")


def build_name(author, title, year, suffix):
    parts = []
    if author:
        parts.append(safe_name(author))
    if title:
        parts.append(safe_name(title))
    name = " - ".join(parts) if parts else None
    if name and year:
        name = f"{name} ({year})"
    if not name:
        return None
    return name + suffix


# --- epub metadata rewriting (OPF only, everything else kept byte-for-byte) ---


def find_opf(zf):
    container = zf.read("META-INF/container.xml").decode("utf-8")
    match = re.search(r'full-path="([^"]+)"', container)
    return match.group(1) if match else None


def set_opf_metadata(opf, updates):
    for key, value in updates.items():
        tag = f"dc:{DC_WRITE_TAGS[key]}"
        pattern = re.compile(rf"(<{tag}\b[^>]*>)(.*?)(</{tag}>)", re.S)
        if pattern.search(opf):
            opf = pattern.sub(
                lambda m, v=escape(value): m.group(1) + v + m.group(3),
                opf,
                count=1,
            )
        else:
            element = f"<{tag}>{escape(value)}</{tag}>"
            opf = opf.replace(
                "</metadata>", f"    {element}\n  </metadata>", 1
            )
    return opf


def rewrite_zip(path, replacements, additions=None):
    additions = additions or {}
    tmp = path.with_name(path.name + ".tmp")
    with zipfile.ZipFile(path) as src, zipfile.ZipFile(tmp, "w") as dst:
        for item in src.infolist():
            data = replacements.get(item.filename)
            if data is None:
                data = src.read(item.filename)
            dst.writestr(item, data)
        for name, data in additions.items():
            dst.writestr(name, data)
    tmp.replace(path)


def read_epub_fields(path):
    book = epub.read_epub(str(path))
    authors = []
    for value, _attrs in book.get_metadata("DC", "creator"):
        authors.extend(author_list_from_text(value))
    return {
        "title": meta(book, "title"),
        "author_list": authors,
        "date": meta(book, "date"),
    }


def update_epub_metadata(path, updates):
    with zipfile.ZipFile(path) as zf:
        opf_name = find_opf(zf)
        if not opf_name:
            print("no OPF found in epub, skipping metadata update")
            return False
        opf = zf.read(opf_name).decode("utf-8")

    new_opf = set_opf_metadata(opf, updates)
    if new_opf == opf:
        return False
    rewrite_zip(path, {opf_name: new_opf.encode("utf-8")})
    return True


# --- epub cover embedding ---

# image magic -> (EPUB media type, file extension)
COVER_TYPES = (
    (b"\x89PNG\r\n\x1a\n", "image/png", "png"),
    (b"\xff\xd8\xff", "image/jpeg", "jpg"),
    (b"GIF87a", "image/gif", "gif"),
    (b"GIF89a", "image/gif", "gif"),
)


def cover_image_type(data):
    """Return (MEDIA-TYPE, EXTENSION) for an image, or (None, None)."""
    for magic, media, extension in COVER_TYPES:
        if data.startswith(magic):
            return media, extension
    return None, None


def tag_attr(tag, name):
    """Return the value of attribute NAME in a single XML TAG, or None."""
    match = re.search(rf'\b{re.escape(name)}\s*=\s*"([^"]*)"', tag)
    return match.group(1) if match else None


def opf_items(opf):
    """Return the `<item ...>' tags of an OPF document."""
    return re.findall(r"<item\b[^>]*>", opf)


def opf_cover_item(opf):
    """Return the manifest item that currently represents the cover, or None."""
    items = opf_items(opf)
    for meta_tag in re.findall(r"<meta\b[^>]*>", opf):
        if (tag_attr(meta_tag, "name") or "").lower() == "cover":
            cover_id = tag_attr(meta_tag, "content")
            for item in items:
                if tag_attr(item, "id") == cover_id:
                    return item
    for item in items:
        if "cover-image" in (tag_attr(item, "properties") or "").split():
            return item
    for item in items:
        media = tag_attr(item, "media-type") or ""
        href = tag_attr(item, "href") or ""
        if media.startswith("image/") and "cover" in href.lower():
            return item
    return None


def opf_set_cover_meta(opf, cover_id):
    """Point the OPF `<meta name="cover">' entry at COVER-ID."""
    updated = False

    def repl(match):
        nonlocal updated
        tag = match.group(0)
        if (tag_attr(tag, "name") or "").lower() != "cover":
            return tag
        updated = True
        if re.search(r'\bcontent="[^"]*"', tag):
            return re.sub(
                r'\bcontent="[^"]*"',
                f'content="{escape(cover_id)}"',
                tag,
                count=1,
            )
        if tag.endswith("/>"):
            return tag[:-2] + f' content="{escape(cover_id)}"/>'
        return tag[:-1] + f' content="{escape(cover_id)}">'

    new_opf = re.sub(r"<meta\b[^>]*>", repl, opf)
    if updated:
        return new_opf
    if "</metadata>" not in new_opf:
        return new_opf
    return new_opf.replace(
        "</metadata>",
        f'    <meta name="cover" content="{escape(cover_id)}"/>\n  </metadata>',
        1,
    )


def opf_set_cover_property(opf, cover_id):
    """Add `properties="cover-image"' to the manifest item COVER-ID."""
    def repl(match):
        tag = match.group(0)
        if tag_attr(tag, "id") != cover_id:
            return tag
        properties = (tag_attr(tag, "properties") or "").split()
        if "cover-image" in properties:
            return tag
        properties.append("cover-image")
        value = " ".join(properties)
        if re.search(r'\bproperties="[^"]*"', tag):
            return re.sub(
                r'\bproperties="[^"]*"',
                f'properties="{value}"',
                tag,
                count=1,
            )
        if tag.endswith("/>"):
            return tag[:-2] + f' properties="{value}"/>'
        return tag[:-1] + f' properties="{value}">'

    return re.sub(r"<item\b[^>]*>", repl, opf)


def opf_clear_cover_property(opf, keep_id):
    """Remove `cover-image' from every manifest item except KEEP-ID."""
    def repl(match):
        tag = match.group(0)
        if tag_attr(tag, "id") == keep_id:
            return tag
        properties = (tag_attr(tag, "properties") or "").split()
        if "cover-image" not in properties:
            return tag
        properties = [item for item in properties if item != "cover-image"]
        if properties:
            return re.sub(
                r'\bproperties="[^"]*"',
                f'properties="{" ".join(properties)}"',
                tag,
                count=1,
            )
        return re.sub(r'\s+properties="[^"]*"', "", tag, count=1)

    return re.sub(r"<item\b[^>]*>", repl, opf)


def opf_unique_id(opf, base):
    """Return BASE, or BASE-N, as a manifest id not used in OPF."""
    ids = {tag_attr(item, "id") for item in opf_items(opf)}
    candidate = base
    number = 1
    while candidate in ids:
        candidate = f"{base}-{number}"
        number += 1
    return candidate


def zip_entry_name(opf_name, href):
    """Return the archive entry for OPF-relative HREF."""
    return posixpath.normpath(
        posixpath.join(posixpath.dirname(opf_name), unquote(href))
    )


def embed_epub_cover(path, image_path):
    """Write IMAGE-PATH into the EPUB at PATH as its cover.

    Return True when the archive was rewritten.  The existing cover item
    is reused when its media type matches; otherwise a new manifest item
    is added and the OPF `cover' meta is pointed at it.
    """
    data = Path(image_path).read_bytes()
    media, extension = cover_image_type(data)
    if not media:
        print(f"skip cover: {image_path} is not a PNG, JPEG or GIF image")
        return False
    with zipfile.ZipFile(path) as zf:
        opf_name = find_opf(zf)
        if not opf_name:
            print("no OPF found in epub, skipping cover update")
            return False
        opf = zf.read(opf_name).decode("utf-8")
        names = set(zf.namelist())
        cover_item = opf_cover_item(opf)
        replacements = {}
        additions = {}
        if cover_item is not None:
            cover_id = tag_attr(cover_item, "id")
            href = tag_attr(cover_item, "href")
            entry = zip_entry_name(opf_name, href) if href else None
            if (entry and entry in names
                    and tag_attr(cover_item, "media-type") == media):
                replacements[entry] = data
                new_opf = opf_set_cover_meta(opf, cover_id)
                new_opf = opf_set_cover_property(new_opf, cover_id)
                new_opf = opf_clear_cover_property(new_opf, cover_id)
                if new_opf != opf:
                    replacements[opf_name] = new_opf.encode("utf-8")
                if zf.read(entry) == data and new_opf == opf:
                    return False
                rewrite_zip(path, replacements)
                return True
        cover_id = opf_unique_id(opf, "cover-image")
        name = f"cover.{extension}"
        number = 1
        while posixpath.join(posixpath.dirname(opf_name), name) in names:
            name = f"cover-{number}.{extension}"
            number += 1
        entry = posixpath.normpath(
            posixpath.join(posixpath.dirname(opf_name), name)
        )
        new_opf = opf.replace(
            "</manifest>",
            f'    <item id="{escape(cover_id)}" href="{escape(name)}" '
            f'media-type="{media}" properties="cover-image"/>\n  </manifest>',
            1,
        )
        if new_opf == opf:
            print("no manifest found in epub, skipping cover update")
            return False
        new_opf = opf_set_cover_meta(new_opf, cover_id)
        new_opf = opf_clear_cover_property(new_opf, cover_id)
        rewrite_zip(
            path,
            {opf_name: new_opf.encode("utf-8")},
            {entry: data},
        )
        return True


# --- pdf metadata through exiftool ---


def require_exiftool():
    if shutil.which("exiftool") is None:
        sys.exit(
            "exiftool not found; install it to update PDF metadata "
            "(e.g. `brew install exiftool`)"
        )


def exiftool_json(path, tags):
    require_exiftool()
    command = ["exiftool", "-j", *[f"-{tag}" for tag in tags], str(path)]
    result = subprocess.run(command, capture_output=True, text=True)
    if result.returncode != 0:
        sys.exit(f"exiftool failed: {(result.stderr or result.stdout).strip()}")
    try:
        entries = json.loads(result.stdout or "[{}]")
    except json.JSONDecodeError:
        sys.exit(f"cannot parse exiftool output for {path}")
    return entries[0] if entries else {}


def read_pdf_fields(path):
    """Read title/author/date from a PDF, mapping them onto info.yaml keys."""
    data = exiftool_json(
        path, ["Title", "Author", "CreateDate", "ModifyDate"]
    )
    if data.get("Error"):
        sys.exit(f"cannot read {path}: {data['Error']}")
    date = iso_date(data.get("CreateDate")) or iso_date(
        data.get("ModifyDate")
    )
    return {
        "title": as_text(data.get("Title")),
        "author_list": author_list_from_text(as_text(data.get("Author"))),
        "date": date,
    }


def update_pdf_metadata(path, updates):
    """Write info.yaml values into a PDF with exiftool.

    Return True when the file was written.  Dates that a PDF cannot
    hold (year or year-month only) are skipped with a message.
    """
    require_exiftool()
    command = ["exiftool", "-overwrite_original"]
    changed = []
    for key, tag in PDF_WRITE_TAGS.items():
        value = updates.get(key)
        if not value:
            continue
        if key == "date":
            value = pdf_date(value)
            if not value:
                print(
                    f"skip pdf date {updates['date']!r}: "
                    "a PDF date needs at least a day (YYYY-MM-DD)"
                )
                continue
        command.append(f"-{tag}={value}")
        changed.append(key)
    if not changed:
        return False
    command.append(str(path))
    result = subprocess.run(command, capture_output=True, text=True)
    if result.returncode != 0:
        sys.exit(f"exiftool failed: {(result.stderr or result.stdout).strip()}")
    return True


# --- dispatch ---


def file_kind(path):
    suffix = path.suffix.lower()
    if suffix == ".epub":
        return "epub"
    if suffix == ".pdf":
        return "pdf"
    sys.exit(f"unsupported file type {path.suffix!r}; expected .epub or .pdf")


def read_fields(path, kind):
    if kind == "epub":
        return read_epub_fields(path)
    return read_pdf_fields(path)


def values_match(kind, key, left, right):
    """Return non-nil when LEFT and RIGHT are the same metadata value.

    PDF dates are compared in their exiftool form so that a date-only
    `info.yaml' value is not rewritten on every run just because the
    PDF stores its midnight equivalent.
    """
    if left == right:
        return True
    if kind == "pdf" and key == "date":
        left, right = pdf_date(left), pdf_date(right)
        return left is not None and left == right
    return False


def write_fields(path, kind, updates):
    updates = dict(updates)
    authors = updates.pop("author_list", None)
    if authors:
        updates["author"] = author_list_to_text(authors)
    if kind == "epub":
        return update_epub_metadata(path, updates)
    return update_pdf_metadata(path, updates)


def parse_args(argv):
    """Return (FILES, COVER) after removing the --embed-cover option."""
    args = list(argv[1:])
    cover = None
    if "--embed-cover" in args:
        index = args.index("--embed-cover")
        if index + 1 >= len(args):
            sys.exit("--embed-cover needs an image file")
        cover = Path(args[index + 1])
        del args[index:index + 2]
    return args, cover


def main():
    args, cover = parse_args(sys.argv)
    if cover is not None:
        if len(args) != 1:
            sys.exit(
                "usage: papis_book_metadata.py --embed-cover IMAGE <file.epub>"
            )
        path = Path(args[0])
        if path.suffix.lower() != ".epub":
            sys.exit("--embed-cover supports only EPUB files")
        if not cover.is_file():
            sys.exit(f"cover image not found: {cover}")
        if embed_epub_cover(path, cover):
            print(f"epub cover updated: {path}")
        else:
            print("epub cover unchanged")
        return

    if len(args) != 1:
        sys.exit("usage: papis_book_metadata.py <file.epub|file.pdf>")

    path = Path(args[0])
    kind = file_kind(path)
    print(path)
    fields = read_fields(path, kind)

    for key, value in fields.items():
        print(f"{kind} {key}:", value if value is not None else "(none)")

    # info.yaml wins over the embedded metadata when both are present
    info_path = path.parent / "info.yaml"
    existing = {}
    if info_path.exists():
        loaded = yaml.safe_load(info_path.read_text(encoding="utf-8"))
        if isinstance(loaded, dict):
            existing = loaded
    info = {k: v for k, v in fields.items() if v}
    for key in DC_TAGS:
        value = as_text(existing.get(key))
        if value:
            info[key] = value

    existing_authors = existing.get("author_list")
    if not isinstance(existing_authors, list) or not existing_authors:
        existing_authors = author_list_from_text(as_text(existing.get("author")))
    if existing_authors:
        info["author_list"] = existing_authors

    # keep info.yaml's `date` and `year` in agreement (date joined by file)
    file_date = fields.get("date")
    info["date"], date_year = reconcile_date(
        as_text(existing.get("date")) or file_date,
        existing.get("year"),
        file_date,
    )
    if date_year:
        info["year"] = date_year
    else:
        info.pop("year", None)
    if info.get("date"):
        print(f"date {info['date']} (year {date_year or '(none)'})")

    # --- push info.yaml values back into the file when they differ ---
    diff = {}
    for key in DC_TAGS:
        value = info.get(key)
        if not value or values_match(kind, key, value, fields.get(key)):
            continue
        if kind == "pdf" and key == "date" and pdf_date(value) is None:
            print(
                f"skip pdf date {value!r}: "
                "a PDF date needs at least a day (YYYY-MM-DD)"
            )
            continue
        diff[key] = value
    info_authors = info.get("author_list") or []
    file_authors = fields.get("author_list") or []
    if info_authors and info_authors != file_authors:
        diff["author_list"] = info_authors
    if diff:
        for key, value in diff.items():
            print(f"sync {kind} {key}: {fields.get(key)!r} -> {value!r}")
        if write_fields(path, kind, diff):
            print(f"{kind} metadata updated")
        else:
            print(f"{kind} metadata unchanged")
        # reload so later steps see the new values
        fields = read_fields(path, kind)

    # --- rename file to "author - title (year).ext" ---
    new_name = build_name(
        primary_author(info.get("author_list")),
        info.get("title"),
        info.get("year"),
        path.suffix,
    )
    new_path = path
    if new_name and new_name != path.name:
        new_path = path.with_name(new_name)
        if new_path.exists():
            print(f"skip rename: {new_path.name} already exists")
            new_path = path
        else:
            path.rename(new_path)
            print(f"renamed: {path.name} -> {new_path.name}")

    # --- write metadata, point info.yaml `files` at the new name ---
    if info:
        existing.pop("creator", None)  # not a papis key
        if info.get("author_list"):
            existing.pop("author", None)
        existing.update(info)
        if isinstance(info.get("year"), str) and info["year"].isdigit():
            existing["year"] = int(info["year"])  # papis-style numeric year
        files = existing.get("files")
        if isinstance(files, list) and files:
            existing["files"] = [
                new_path.name if old == path.name else old for old in files
            ]
        else:
            existing["files"] = [new_path.name]
        info_path.write_text(
            yaml.safe_dump(existing, allow_unicode=True, sort_keys=False),
            encoding="utf-8",
        )
        print("written:", info_path)
    else:
        print("no metadata found, nothing written")


if __name__ == "__main__":
    main()
