# papis.el

An Emacs frontend for [Papis](https://github.com/papis/papis), split into
layers:

- **`papis.el`** is a client for the JSON API exposed by `papis serve`.  It
  starts the server on demand, runs queries, and returns plain Lisp data.  It
  knows nothing about buffers, faces, or UI frameworks.
- **`papis-etaf.el`** is an optional cover gallery built with
  [ETAF](https://etaf.top/introduction/).  It renders books as a responsive
  grid of covers, titles, authors and tag pills, with a selected-document
  action bar.

`scripts/papis_dump.py` is a small fallback helper for a Papis 0.16 server
bug; it is never used in the normal path.

## Requirements

- Emacs 29.1 or newer
- Papis 0.16 or newer, with the `papis` executable on `exec-path`
- [ETAF](https://etaf.top/introduction/) 0.3.5 or newer, only for
  `papis-etaf.el`
- [ExifTool](https://exiftool.org) for updating PDF metadata (for example
  `brew install exiftool`)
- Optional for cover thumbnails: `pdftoppm` (Poppler) or `mutool` renders the
  first page of PDF books; EPUB covers are read directly from the archive

ETAF is available from its own package archive:

```elisp
(add-to-list 'package-archives '("etaf" . "https://etaf.top/packages/"))
```

## Setup

Put the `.el` files somewhere on `load-path`, then:

```elisp
(require 'papis-etaf)

;; Optional
(setq papis-server-port 8888)
(setq papis-default-library "books")
```

With `use-package`:

```elisp
(use-package papis-etaf
  :load-path "/path/to/papis.el"
  :commands (papis-etaf-open))
```

Start the ETAF cover gallery with `M-x papis-etaf-open`.

If no server is listening on `papis-server-port`, `papis serve` is started as a
child process and reused afterwards.  A server started by another program is
also reused.  Use `M-x papis-stop-server` to stop only the server started by
this Emacs.

## Cover gallery (ETAF)

`M-x papis-etaf-open` opens `*papis-etaf*`, a responsive gallery of book
covers.  Each card shows the cover, title, author and the first tag; selecting
a card enables the open, folder, info, BibTeX and URL actions below the grid.
Covers are extracted from EPUB metadata, rendered from the first PDF page when
`pdftoppm` or `mutool` is available, or generated as an SVG cover from the
document metadata.  The cache lives in `papis-etaf-cover-directory`
(`~/.cache/papis/etaf-covers` by default); use `M-x
papis-etaf-clear-cover-cache` to rebuild it.

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
| `b`       | Copy a BibTeX entry              |
| `c`       | Copy a human-readable reference  |
| `q`       | Quit the gallery                 |

## Papis API used by `papis.el`

| Function                        | API endpoint |
|---------------------------------|--------------|
| `papis-libraries`               | `GET /api/library` |
| `papis-library`                 | `GET /api/library/<library>` |
| `papis-documents` / `papis-query` | `GET /api/library/<library>/document[/<query>]` |
| `papis-document-format`         | `GET /api/library/<library>/document/<query>/format/<format>` |
| `papis-document-delete`         | `papis rm` on the command line (the server has no delete route) |
| `papis-document-tag-add` / `papis-document-tag-remove` | `papis tag --add` / `--remove` on the command line |

Documents are returned as `papis-document` structs with accessors such as
`papis-document-title`, `papis-document-author`, `papis-document-files`,
`papis-document-tags`, and the generic `papis-document-get`.

The server lifecycle is managed by `papis-start-server`, `papis-stop-server`,
`papis-restart-server`, and `papis-ensure-server`.

### Date serialization workaround

Papis 0.16 can fail to serialize a document whose YAML `date` field was parsed
as a date object; the server then answers with:

```
Object of type date is not JSON serializable
```

Since this happens inside Papis, the client works around it when
`papis-fallback-to-cli` is non-nil (the default):

1. `scripts/papis_dump.py` runs through `papis exec`, using Papis' Python API
   and serializing dates as strings.
2. If the helper is unavailable, `papis-documents` enumerates matching ids with
   `papis list --id` and fetches each document through the HTTP API by id.

## Troubleshooting

If the frontend seems stuck, the echo area shows the current phase: connecting
to Papis, starting the server, or loading documents.  `C-g` aborts a pending
request.  `M-x papis-check` writes a diagnostic buffer with the executable, the
server URL, whether the server responds, the libraries, and the server log.

The Papis server is local, so its requests bypass `url-proxy-services` even
when `http_proxy` is set.  If the server cannot be started, check
`*papis-server*` or run `M-x papis-open-server-log`; a port conflict with
another program is reported there.

Timeouts are controlled by `papis-request-timeout` (one request),
`papis-server-probe-timeout` (liveness check), and
`papis-server-start-timeout` (starting `papis serve`).

## Development

The repository uses [Eldev](https://github.com/doublep/eldev):

```sh
eldev compile
eldev test
```

Tests use Buttercup and cover the parts of `papis.el` that do not require a
running server.
