#!/usr/bin/env -S uv run --script

# /// script
# dependencies = []
# ///
"""Append a file to a Papis document's files metadata."""

import sys


def main():
    from papis.document import from_folder

    if len(sys.argv) != 3:
        sys.exit("usage: papis_add_file.py <document-folder> <file-name>")

    document = from_folder(sys.argv[1])
    name = sys.argv[2]
    # Move NAME to the end so the most recently attached file wins.
    files = [item for item in (document.get("files") or []) if item != name]
    files.append(name)
    document["files"] = files
    document.save()
    print(name)


main()
