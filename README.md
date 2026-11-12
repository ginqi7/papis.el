# papis.el

An Emacs frontend for [Papis](https://github.com/papis/papis), split into
layers:

- **`papis.el`** talks to Papis through the `papis` executable
  (`papis exec` + `scripts/papis_dump.py`) and returns plain Lisp data.  It
  knows nothing about buffers, faces, or UI frameworks.
- **`papis-openlibrary.el`** (optional) looks up book metadata and covers on
  [Open Library](https://openlibrary.org) and turns a result into Papis
  fields or a cover file.  The ETAF gallery uses it for its metadata and
  cover commands.
- **`papis-etaf.el`** is an optional cover gallery built with
  [ETAF](https://etaf.top/introduction/).  It renders books as a responsive
  grid of covers, titles, authors and tag pills, with a selected-document
  action bar.

`scripts/papis_dump.py` dumps documents as JSON for the Emacs frontend.

## Requirements

- Emacs 29.1 or newer
- Papis 0.16 or newer, with the `papis` executable on `exec-path`
- [ETAF](https://etaf.top/introduction/) 0.3.5 or newer, only for
  `papis-etaf.el`
- [ExifTool](https://exiftool.org) for updating PDF metadata (for example
  `brew install exiftool`)
- Optional for cover thumbnails: `pdftoppm` (Poppler) or `mutool` renders the
  first page of PDF books; EPUB covers are read directly from the archive
- Optional for metadata lookup: [openlibrary.el](https://github.com/ginqi7/openlibrary.el)
  with [plz.el](https://github.com/alphapapa/plz.el) on `load-path`

ETAF is available from its own package archive:

```elisp
(add-to-list 'package-archives '("etaf" . "https://etaf.top/packages/"))
```

## Setup

Put the `.el` files somewhere on `load-path`, then:

```elisp
(require 'papis-etaf)

;; Optional
(setq papis-default-library "books")
```

With `use-package`:

```elisp
(use-package papis-etaf
  :load-path "/path/to/papis.el"
  :commands (papis-etaf-open))
```

Start the ETAF cover gallery with `M-x papis-etaf-open`.

No Papis server is needed: queries run the `papis` executable with `papis
exec`, so results come from the current Papis cache and `info.yaml`.

`M-x papis-add-book` adds a book: it prompts for the EPUB/PDF, library, title,
author and year (defaults are parsed from a file name shaped like
`Author - Title (Year).ext`), runs `papis add`, then writes a structured
`author_list` and lets Papis generate the `author` string.


## Cover gallery (ETAF)

`M-x papis-etaf-open` opens `*papis-etaf*`, a responsive gallery of book
covers.  Each card shows the cover, title, author and the first tag; selecting
a card shows its details and the shortcut reference in the bar below the grid,
and all document actions are keyboard shortcuts (there are no action
buttons).  Moving the keyboard cursor to a card (Tab, the arrow keys or a
click) selects that book immediately.
Covers are extracted from EPUB metadata, rendered from the first PDF page when
`pdftoppm` or `mutool` is available, or generated as an SVG cover from the
document metadata.  PDF papers are rendered as typographic covers built from
their title, authors, venue, year and DOI; set
`papis-etaf-paper-cover-style` to `first-page` to use the PDF page instead.
Generated SVG covers and covers extracted or rendered from EPUB/PDF files are
written to the document folder as `cover.svg`, `cover.jpg` or `cover.png` and
appended to its `files` metadata, so they are part of the document rather than
the cache.  `papis-etaf-cover-directory` (`~/.cache/papis/etaf-covers` by
default) is only used when a document has no folder; `M-x
papis-etaf-clear-cover-cache` clears that fallback cache.

`m` prompts for a metadata source: Open Library,
Crossref, arXiv, DBLP, a DOI or PubMed identifier, or the PDF's first page.
Open Library searches by ISBN when the document has one and otherwise by title
and author; the paper sources search Crossref/arXiv/DBLP or import the given
identifier.  After choosing a result, the title, author(s), year and other
fields are written back with `papis update`, so the new metadata is in
`info.yaml` and in the gallery immediately.  Crossref/arXiv/DBLP results are
limited by `papis-paper-search-limit`.

`M` runs `scripts/papis_book_metadata.py` on
the selected book's EPUB or PDF: the script merges the file's metadata with
`info.yaml`, writes both back, renames the file to
`author - title (year).epub` (or `.pdf`) and refreshes the Papis cache.  The
same script is available outside the gallery as `M-x
papis-update-book-metadata`, which prompts for a library and document.

`C` asks whether to generate a cover locally or
download one from Open Library.  A generated cover is written to the document
folder as `cover.svg`; a downloaded cover is saved as `cover.jpg`.  In both
cases the file is appended to the document's `files` metadata, so the most
recently attached cover is the one shown.  When the document has an EPUB file,
a downloaded image is also embedded in the EPUB (manifest `cover-image` item
and OPF `cover` meta), so other readers show it as well.  The Open Library
lookup is also available as `M-x papis-openlibrary-set-cover`.

| Key       | Command                          |
|-----------|----------------------------------|
| `/`       | Read a Papis query               |
| `r`, `g`  | Reload the current query         |
| `s`       | Switch to the next library       |
| `n`, `p`  | Select next / previous document  |
| `RET`     | Activate the focused card        |
| `o`       | Open the selected file           |
| `u`       | Open the selected document's URL |
| `d`       | Open the document folder         |
| `i`       | Open `info.yaml`                 |
| `a`       | Add a book to the current library |
| `t`       | Add a tag to the selected book |
| `m`       | Look up metadata (multiple sources) |
| `M`       | Update EPUB/PDF metadata and rename |
| `C`       | Generate or download a cover |
| `b`       | Copy a BibTeX entry              |
| `c`       | Copy a human-readable reference  |
| `q`       | Quit the gallery                 |

## How `papis.el` talks to Papis

`papis.el` uses these commands:

| Function | CLI implementation |
|---|---|
| `papis-libraries` | `papis list --libraries` |
| `papis-library` | lookup in `papis list --libraries` |
| `papis-documents` / `papis-query` | `papis exec scripts/papis_dump.py LIBRARY QUERY` |
| `papis-document-format` | `papis export --all -f FORMAT` |
| `papis-document-delete` | `papis rm --force --all` |
| `papis-document-tag-add` / `-remove` | `papis tag --add` / `--remove` |
| `papis-document-update` | `papis update --set` |
| `papis-document-add-file` | `papis update --append files` |
| `papis-document-refresh-cache` | `papis cache update` |

Documents are returned as `papis-document` structs with accessors such as
`papis-document-title`, `papis-document-author`, `papis-document-files`,
`papis-document-tags`, and the generic `papis-document-get`.

Functions that change metadata — `papis-document-update`,
`papis-document-add-file`, the tag commands and the book metadata script —
refresh the Papis cache for the document folder afterwards.

### Author metadata

Author changes write the structured `author_list` field (a list of
`{family, given}` mappings) and then let Papis generate the display `author`
string with `papis update --reset author`, using the
`multiple-authors-format` and `multiple-authors-separator` configuration.  The
Open Library command and `scripts/papis_book_metadata.py` both follow this
rule.

After editing `author_list` manually in `info.yaml`, run

```elisp
M-x papis-update-author
```

or, without Emacs:

```sh
papis -l <library> cache update --all
papis -l <library> exec scripts/papis_author.py <document-folder>
papis -l <library> cache update --all
```

Both write only `info.yaml`, so the attached files keep their names.

### Date serialization

`scripts/papis_dump.py` serializes documents with `default=str`, so YAML
`date` values parsed as date objects do not break the JSON output.

## Troubleshooting

`M-x papis-check` writes a diagnostic buffer with the `papis` executable, the
dump script and the libraries.  Queries start a `papis` process each time
(roughly 0.1 s), so the echo area may be quiet while the command runs; set
`papis-debug` to log CLI invocations.

## Development

The repository uses [Eldev](https://github.com/doublep/eldev):

```sh
eldev compile
eldev test
```

Tests use Buttercup and cover the parts of `papis.el` that do not need a
running Papis executable.
