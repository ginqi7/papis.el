#!/usr/bin/env -S uv run --script

# /// script
# dependencies = []
# ///
"""Regenerate a Papis document's `author` field from `author_list`."""

import sys


def main():
    from papis.document import author_list_to_author, from_folder

    if len(sys.argv) != 2:
        sys.exit("usage: papis_author.py <document-folder>")

    document = from_folder(sys.argv[1])
    if not document.get("author_list"):
        print("no author_list; nothing to do")
        return

    document["author"] = author_list_to_author(document)
    document.save()
    print(document["author"])


main()
