# papis.el

An Emacs frontend for [Papis](https://github.com/papis/papis), split into two
layers:

- **`papis.el`** is a client for the JSON API exposed by `papis serve`.  It
  starts the server on demand, runs queries, and returns plain Lisp data.  It
  knows nothing about buffers, faces, or UI frameworks.
- **`papis-vui.el`** consumes only the functions in `papis.el` and renders them
  as a [vui.el](https://github.com/d12frosted/vui.el) interface: search,
  library switching, tag filters, sorting, a document table, and a detail pane.

`scripts/papis_dump.py` is a small fallback helper for a Papis 0.16 server
bug; it is never used in the normal path.

## Requirements

- Emacs 29.1 or newer
- Papis 0.16 or newer, with the `papis` executable on `exec-path`
- [vui.el](https://github.com/d12frosted/vui.el) 1.4.0 or newer

## Setup

Put the two `.el` files somewhere on `load-path`, then:

```elisp
(require 'papis-vui)

;; Optional
(setq papis-server-port 8888)
(setq papis-default-library "books")
```

With `use-package`:

```elisp
(use-package papis
  :load-path "/path/to/papis.el"
  :commands (papis-vui-open))

(use-package papis-vui
  :load-path "/path/to/papis.el"
  :commands (papis-vui-open))
```

Start the frontend with `M-x papis-vui-open` (or `M-x papis-vui`).

If no server is listening on `papis-server-port`, `papis serve` is started as a
child process and reused afterwards.  A server started by another program is
also reused.  Use `M-x papis-stop-server` to stop only the server started by
this Emacs.

## Key bindings

| Key       | Command                          |
|-----------|----------------------------------|
| `g`, `r`  | Reload the current query         |
| `/`       | Read a Papis query               |
| `l`       | Switch library                   |
| `s`       | Choose the sort field            |
| `n`, `p`  | Select next / previous document  |
| `o`       | Open the selected file           |
| `u`       | Open the selected document's URL |
| `d`       | Open the document folder         |
| `i`       | Open `info.yaml`                 |
| `b`       | Copy a BibTeX entry              |
| `c`       | Copy a human-readable reference  |
| `e`       | Manually edit the metadata       |
| `m`       | Update EPUB metadata and rename  |
| `D`       | Delete the selected document     |
| `TAB`     | Move between buttons and fields  |
| `q`       | Quit the window                  |

All metadata actions are also available as buttons in the detail pane.
`D` (or the **Delete** button) asks for confirmation and then runs
`papis rm --force --all --doc-folder <folder>` (falling back to
`papis_id:<id>` when the folder is unknown), so the folder is removed from
disk and cannot be recovered.  Because `papis rm` exits with status 0 even
when it matches nothing, the folder is checked afterwards: if it is still
there, the command fails with the exact `papis` command and output instead
of pretending the document was deleted.  Set `papis-debug` to non-nil to
log every `papis rm` invocation.

`e` (or the **Edit** button) opens `info.yaml` in a dedicated
`papis-vui-metadata-edit-mode` buffer for manual editing.  `C-c C-c`
writes the metadata, updates and renames the EPUB when the document has one,
refreshes the Papis cache and returns to the frontend; `C-c C-k` cancels.

`m` (or the **Auto metadata** button) runs
`scripts/papis_book_metadata.py` on the selected document's EPUB file.  The
script reads the EPUB's Dublin Core metadata, merges it with `info.yaml`,
writes both back, renames the EPUB to `author - title (year).epub`, and the
frontend then refreshes the Papis cache (`papis cache update` for the
document folder, or `papis cache update-newer` when the folder is unknown)
so the server returns the new metadata.  The script uses `uv` (through its
shebang) and declares its `ebooklib` and `pyyaml` dependencies itself, so no
extra setup is needed after `uv` is installed.  Customize
`papis-book-metadata-script` to use a different script.

## Papis API used by `papis.el`

| Function                        | API endpoint |
|---------------------------------|--------------|
| `papis-libraries`               | `GET /api/library` |
| `papis-library`                 | `GET /api/library/<library>` |
| `papis-documents` / `papis-query` | `GET /api/library/<library>/document[/<query>]` |
| `papis-document-format`         | `GET /api/library/<library>/document/<query>/format/<format>` |
| `papis-document-delete`         | `papis rm` on the command line (the server has no delete route) |

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
