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
import re
import shutil
import subprocess
import sys
import zipfile
from pathlib import Path
from xml.sax.saxutils import escape

import yaml

UNSAFE = re.compile(r'[/\\*?"<>|\x00-\x1f]')
COLON = re.compile(r"\s*:\s*")

# info.yaml key -> OPF/DC element name
DC_TAGS = {"title": "title", "author": "creator", "date": "date"}

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


def primary_author(author):
    """'A; B' -> 'A' (translator/co-author list trimmed to the main author)."""
    return author.split(";")[0].strip() if author else None


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
        tag = f"dc:{DC_TAGS[key]}"
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


def rewrite_zip(path, replacements):
    tmp = path.with_name(path.name + ".tmp")
    with zipfile.ZipFile(path) as src, zipfile.ZipFile(tmp, "w") as dst:
        for item in src.infolist():
            data = replacements.get(item.filename)
            if data is None:
                data = src.read(item.filename)
            dst.writestr(item, data)
    tmp.replace(path)


def read_epub_fields(path):
    book = epub.read_epub(str(path))
    return {key: meta(book, dc) for key, dc in DC_TAGS.items()}


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
        "author": as_text(data.get("Author")),
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
    if kind == "epub":
        return update_epub_metadata(path, updates)
    return update_pdf_metadata(path, updates)


def main():
    if len(sys.argv) < 2:
        sys.exit("usage: papis_book_metadata.py <file.epub|file.pdf>")

    path = Path(sys.argv[1])
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
        primary_author(info.get("author")),
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
