"""Dump Papis documents as JSON for the Emacs frontend.

Run through Papis so that this script sees the Python environment Papis is
installed in::

    papis exec scripts/papis_dump.py LIBRARY [QUERY]

The JSON is printed on a single line prefixed with ``PAPIS_JSON:`` so that
Papis' own configuration warnings, which also go to stdout, can be ignored.
"""

from __future__ import annotations


def main() -> None:
    import json
    import sys

    from papis.api import get_all_documents_in_lib, get_documents_in_lib
    from papis.document import to_dict

    library = sys.argv[1]
    query = sys.argv[2] if len(sys.argv) > 2 else ""

    if query.strip():
        documents = get_documents_in_lib(library, query)
    else:
        documents = get_all_documents_in_lib(library)

    # Give Emacs absolute paths and, even for documents without files, the
    # folder they live in.  This mirrors what `papis serve' does for /api.
    for document in documents:
        document["files"] = document.get_files()
        folder = document.get_main_folder()
        if folder:
            document["_papis_local_folder"] = folder

    print(
        "PAPIS_JSON:"
        + json.dumps([to_dict(document) for document in documents], default=str)
    )


# `papis exec' runs this file with `exec', where __name__ is not
# "__main__", so call main unconditionally.
main()
