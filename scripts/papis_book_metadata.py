#!/usr/bin/env -S uv run --script

# /// script
# dependencies = [
#   "ebooklib",
#   "pyyaml",
# ]
# ///
from ebooklib import epub

import re
import sys
import zipfile
from pathlib import Path
from xml.sax.saxutils import escape

import yaml

UNSAFE = re.compile(r'[/\\*?"<>|\x00-\x1f]')
COLON = re.compile(r"\s*:\s*")

# info.yaml key -> OPF/DC element name
DC_TAGS = {"title": "title", "author": "creator", "date": "date"}


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


def reconcile_date(date, year, epub_date=None):
    """Make (date, year) agree, honouring whichever field was edited.

    `date` may be a full date, a year-month or a year only, e.g.
    '2026-09-23T12:55:55Z', '2026-09' or '2026'. `year` is normally the
    bare 4-digit value stored in info.yaml. When the two disagree, the
    field the user changed relative to the epub wins; if both changed,
    `date` wins (and a warning is printed).
    """
    date = as_text(date)
    year = as_text(year)
    if not year or not re.fullmatch(r"\d{4}", year):
        year = None

    date_year = year_of(date)
    if date and year and date_year != year:
        date_changed = date != epub_date
        year_changed = year != year_of(epub_date)
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


if len(sys.argv) < 2:
    sys.exit("usage: ,ebook.py <file.epub>")

path = Path(sys.argv[1])
print(path)
book = epub.read_epub(str(path))

epub_fields = {key: meta(book, dc) for key, dc in DC_TAGS.items()}

for key, value in epub_fields.items():
    print(f"epub {key}:", value if value is not None else "(none)")

# info.yaml wins over the epub when both are present
info_path = path.parent / "info.yaml"
existing = {}
if info_path.exists():
    loaded = yaml.safe_load(info_path.read_text(encoding="utf-8"))
    if isinstance(loaded, dict):
        existing = loaded
info = {k: v for k, v in epub_fields.items() if v}
for key in DC_TAGS:
    value = as_text(existing.get(key))
    if value:
        info[key] = value

# keep info.yaml's `date` and `year` in agreement (date joined by epub)
epub_date = epub_fields.get("date")
info["date"], date_year = reconcile_date(
    as_text(existing.get("date")) or epub_date,
    existing.get("year"),
    epub_date,
)
if date_year:
    info["year"] = date_year
else:
    info.pop("year", None)
if info.get("date"):
    print(f"date {info['date']} (year {date_year or '(none)'})")

# --- push info.yaml values back into the epub metadata when they differ ---
diff = {
    key: info[key]
    for key in DC_TAGS
    if info.get(key) and info[key] != epub_fields.get(key)
}
if diff:
    for key, value in diff.items():
        print(f"sync epub {key}: {epub_fields.get(key)!r} -> {value!r}")
    if update_epub_metadata(path, diff):
        print("epub metadata updated")
    else:
        print("epub metadata unchanged")
    # reload so later steps see the new values
    book = epub.read_epub(str(path))
    epub_fields = {key: meta(book, dc) for key, dc in DC_TAGS.items()}

# --- rename epub to "author - title (year).ext" ---
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
