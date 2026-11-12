;;; papis.el --- Talk to Papis from Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/ginqi7/papis.el
;; Package-Requires: ((emacs "29.1"))
;; Keywords: bib, tex, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is the low-level half of the Emacs frontend for
;; https://github.com/papis/papis.  It reads through the `papis'
;; executable: `papis exec' runs `scripts/papis_dump.py', which returns
;; documents as JSON, and libraries come from `papis list --libraries'.
;; Writes use CLI commands as well (rm, tag, update, cache).
;;
;; All functions in this file return plain Lisp data.  Nothing here
;; knows about buffers or faces; `papis-etaf.el' renders them.
;;
;; `scripts/papis_dump.py' serializes documents with `default=str', so
;; YAML `date' values parsed as date objects do not break the JSON
;; output.
;;
;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)

(defgroup papis nil
  "Emacs frontend for Papis."
  :group 'applications
  :prefix "papis-")

;;; Errors

(define-error 'papis-error "Papis error")

(defun papis-error-message (error)
  "Return a clean message string for the error value ERROR.

Custom errors carry their message as the first data element, which
`error-message-string' would print with surrounding quotes."
  (let ((data (cdr-safe error)))
    (if (and data
             (stringp (car data))
             (memq (car-safe error)
                   '(papis-error)))
        (car data)
      (error-message-string error))))

;;; Custom variables

(defcustom papis-executable (or (executable-find "papis") "papis")
  "The Papis executable used for CLI calls."
  :type 'string
  :group 'papis)

(defcustom papis-debug nil
  "Whether to log Papis CLI invocations in the echo area."
  :type 'boolean
  :group 'papis)

(defcustom papis-default-library nil
  "Library used when none is given explicitly.

When nil, the value of the PAPIS_LIB environment variable and then
the first library reported by Papis are used."
  :type '(choice (const :tag "Automatic" nil) string)
  :group 'papis)

(defcustom papis-info-file-name "info.yaml"
  "Name of the metadata file inside a document folder."
  :type 'string
  :group 'papis)

(defcustom papis-dump-script
  (expand-file-name
   "scripts/papis_dump.py"
   (or (and load-file-name (file-name-directory load-file-name))
       (and buffer-file-name (file-name-directory buffer-file-name))
       default-directory))
  "Python helper that dumps documents as JSON for the CLI backend.

It is run with `papis exec' so that it sees the Python environment
Papis is installed in."
  :type 'string
  :group 'papis)

(defcustom papis-book-metadata-script
  (expand-file-name
   "scripts/papis_book_metadata.py"
   (or (and load-file-name (file-name-directory load-file-name))
       (and buffer-file-name (file-name-directory buffer-file-name))
       default-directory))
  "Python helper that updates book metadata and renames book files.

It reads the metadata of an EPUB or PDF, merges it with
`info.yaml', writes the result back to both files, and renames the
file to \"author - title (year).epub\" or \"... .pdf\".  EPUB
metadata is updated in place; PDF metadata is written with
ExifTool, which must be installed.  The script lives in this
repository, so the default normally needs no changes."
  :type 'string
  :group 'papis)

(defcustom papis-open-file-function #'find-file
  "Function called with a file name to open a document file.

Use something like `browse-url' if your documents should be opened by
an external program."
  :type 'function
  :group 'papis)

(defcustom papis-document-field-order
  '("title" "author" "year" "date" "type" "journal" "booktitle"
    "publisher" "volume" "number" "pages" "doi" "isbn" "url" "doc_url"
    "eprint" "tags" "abstract" "note" "notes" "time-added")
  "Preferred order of document fields when displaying metadata."
  :type '(repeat string)
  :group 'papis)

(defcustom papis-tags '("reading" "read")
  "Tags offered when adding a tag to a document.

These are only completion candidates: `papis-document-tag-add'
accepts any tag, and tags already used in the library are offered
too.  Customize this list to preset the tags that are useful to
you."
  :type '(repeat string)
  :group 'papis)

;;; Diagnostics

(defun papis-check ()
  "Display a diagnostic report for the Papis integration."
  (interactive)
  (let ((buffer (get-buffer-create "*papis-check*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Emacs: %s\n" emacs-version)
              (format "papis executable: %s\n"
                      (or (and papis-executable
                               (file-executable-p papis-executable)
                               papis-executable)
                          (executable-find "papis")
                          "not found"))
              (format "dump script: %s\n"
                      (if (file-exists-p papis-dump-script)
                          papis-dump-script
                        (format "%s (missing)" papis-dump-script)))
              "\nLibraries:\n")
      (condition-case err
          (insert (format "%S\n" (papis-libraries)))
        (error
         (insert (format "ERROR: %s\n" (papis-error-message err)))))
      (special-mode)
      (goto-char (point-min)))
    (display-buffer buffer)))

;;; Libraries

(defun papis--library-lines ()
  "Return the `papis list --libraries' output as non-empty lines."
  (let ((output (papis--clean-cli-output
                 (papis--call-cli '("list" "--libraries")))))
    (seq-filter (lambda (line) (not (string-empty-p line)))
                (mapcar #'string-trim (split-string output "\n")))))

(defun papis--libraries-via-cli ()
  "Return the names of all Papis libraries using the CLI."
  (delq nil
        (mapcar (lambda (line)
                  (let ((name (car (split-string line "[ \t]+" t))))
                    (unless (string-empty-p name) name)))
                (papis--library-lines))))

(defun papis--library-via-cli (library)
  "Return a plist with :name and :path for LIBRARY using the CLI."
  (let* ((line (seq-find (lambda (line)
                           (string-prefix-p (concat library " ") line))
                         (papis--library-lines)))
         (fields (and line (split-string line "[ \t]+" t))))
    (unless fields
      (signal 'papis-error
              (list (format "Unknown Papis library: %s" library))))
    (list :name (car fields)
          :path (expand-file-name
                 (string-trim (substring line (length (car fields))))))))

(defun papis-libraries ()
  "Return the names of all Papis libraries."
  (papis--libraries-via-cli))

(defun papis-library (library)
  "Return a plist with :name and :path for LIBRARY."
  (papis--library-via-cli library))

(defvar papis--default-library-cache :unset
  "Cached value of Papis' `default-library' configuration.")

(defun papis-configured-default-library ()
  "Return the `default-library' value from the Papis configuration.

Ask the CLI once and cache the answer for the rest of the session."
  (when (eq papis--default-library-cache :unset)
    (setq papis--default-library-cache
          (condition-case nil
              (let ((value (string-trim
                            (papis--clean-cli-output
                             (papis--call-cli '("config" "default-library"))))))
                (unless (string-empty-p value) value))
            (error nil))))
  papis--default-library-cache)

(defun papis-default-library-name (&optional libraries)
  "Return the library to use when none was given.

Check `papis-default-library', the PAPIS_LIB environment variable,
Papis' own `default-library' configuration and finally the first
entry of LIBRARIES.  When LIBRARIES is non-nil, a candidate must be
one of its members."
  (or (and papis-default-library
           (or (null libraries) (member papis-default-library libraries))
           papis-default-library)
      (let ((env (getenv "PAPIS_LIB")))
        (and env
             (not (string-empty-p env))
             (or (null libraries) (member env libraries))
             env))
      (let ((configured (papis-configured-default-library)))
        (and configured
             (or (null libraries) (member configured libraries))
             configured))
      (car libraries)))

(defun papis--default-library ()
  "Return the library to use when none was given."
  (papis-default-library-name (papis-libraries)))

;;; Documents

(cl-defstruct (papis-document (:constructor papis-document--create))
  "A Papis document and the library it came from."
  data
  library)

(defun papis-document-copy (document)
  "Return a copy of DOCUMENT with its own metadata."
  (papis-document--create
   :data (copy-tree (papis-document-data document))
   :library (papis-document-library document)))

(defun papis--value-to-string (value)
  "Convert a JSON metadata VALUE to a human-readable string."
  (cond
   ((null value) "")
   ((stringp value) value)
   ((listp value)
    (string-join (mapcar #'papis--value-to-string value) ", "))
   ((numberp value) (number-to-string value))
   (t (format "%s" value))))

(defun papis--document-key-symbol (key)
  "Return the metadata symbol used for KEY in a document."
  (cond
   ((keywordp key) (intern (substring (symbol-name key) 1)))
   ((stringp key) (intern key))
   (t key)))

(defun papis-document-get (document key &optional default)
  "Return KEY from DOCUMENT's metadata, or DEFAULT.

KEY may be a string, a symbol or a keyword.  JSON object keys are
parsed by Emacs as symbols, so strings are interned for lookup."
  (let* ((key (papis--document-key-symbol key))
         (cell (assoc key (papis-document-data document))))
    (if cell (cdr cell) default)))

(defun papis-document-keys (document)
  "Return the metadata keys of DOCUMENT as strings."
  (mapcar (lambda (cell)
            (let ((key (car cell)))
              (if (symbolp key) (symbol-name key) key)))
          (papis-document-data document)))

(defun papis-document-value-string (document key)
  "Return the value of KEY in DOCUMENT as a string."
  (papis--value-to-string (papis-document-get document key)))

(defun papis-document-id (document)
  "Return the Papis id of DOCUMENT, falling back to its ref."
  (let ((id (papis-document-get document "papis_id")))
    (or (and id (papis--value-to-string id))
        (let ((ref (papis-document-get document "ref")))
          (and ref (papis--value-to-string ref))))))

(defun papis-document-title (document)
  "Return the title of DOCUMENT."
  (let ((title (papis-document-get document "title")))
    (if (and title (not (string-empty-p (papis--value-to-string title))))
        (papis--value-to-string title)
      "Untitled")))

(defun papis-document-author (document)
  "Return the author(s) of DOCUMENT as a string."
  (papis--value-to-string (papis-document-get document "author")))

(defun papis-document-year (document)
  "Return the year of DOCUMENT, or nil."
  (papis-document-get document "year"))

(defun papis-document-year-string (document)
  "Return the year of DOCUMENT as a string."
  (let ((year (papis-document-year document)))
    (cond
     ((null year) "")
     ((numberp year) (number-to-string year))
     (t (papis--value-to-string year)))))

(defun papis-document-isbn (document)
  "Return the first ISBN recorded for DOCUMENT, or nil."
  (let ((value (papis-document-get document "isbn")))
    (cond
     ((null value) nil)
     ((listp value)
      (seq-find (lambda (isbn)
                  (let ((isbn (string-trim (papis--value-to-string isbn))))
                    (unless (string-empty-p isbn) isbn)))
                value))
     (t
      (let ((isbn (string-trim (papis--value-to-string value))))
        (unless (string-empty-p isbn) isbn))))))

(defun papis-document-tags (document)
  "Return the tags of DOCUMENT as a list of strings."
  (let ((tags (papis-document-get document "tags")))
    (cond
     ((null tags) nil)
     ((listp tags)
      (delq nil (mapcar (lambda (tag)
                          (let ((tag (papis--value-to-string tag)))
                            (unless (string-empty-p tag) tag)))
                        tags)))
     ((stringp tags)
      (if (string-match-p "[,;]" tags)
          (mapcar #'string-trim (split-string tags "[,;]+" t "[ \t\n]"))
        (mapcar #'string-trim (split-string tags "[ \t\n]+" t))))
     (t (list (papis--value-to-string tags))))))

(defun papis-document-files (document)
  "Return the attached files of DOCUMENT as a list of absolute paths."
  (let ((files (papis-document-get document "files")))
    (cond
     ((null files) nil)
     ((stringp files) (list files))
     ((listp files)
      (delq nil (mapcar (lambda (file)
                          (let ((file (papis--value-to-string file)))
                            (unless (string-empty-p file) file)))
                        files)))
     (t nil))))

(defun papis-document-file (document)
  "Return the first attached file of DOCUMENT, or nil."
  (car (papis-document-files document)))

(defun papis-document-file-type (document)
  "Return the type of the attached files of DOCUMENT.

The type is the upper-case extension of the first attached file
that has one, e.g. \"PDF\" or \"EPUB\".  Return nil when no file
type can be determined."
  (seq-some (lambda (file)
              (let ((extension (file-name-extension file)))
                (and extension
                     (not (string-empty-p extension))
                     (upcase extension))))
            (papis-document-files document)))

(defun papis-document-directory (document)
  "Return the folder of DOCUMENT, or nil when it cannot be determined."
  (or (when-let* ((file (papis-document-file document)))
        (file-name-directory file))
      (let ((folder (papis-document-get document "_papis_local_folder")))
        (when (and (stringp folder) (not (string-empty-p folder)))
          (file-name-as-directory folder)))))

(defun papis-document-info-file (document)
  "Return the info file of DOCUMENT, or nil."
  (when-let* ((directory (papis-document-directory document)))
    (expand-file-name papis-info-file-name directory)))

(defun papis-document-folder (document)
  "Return the main folder of DOCUMENT, if it can be determined.

Prefer the folder reported by Papis itself (`_papis_local_folder')
so that documents whose files live in a subfolder are not confused
with the document folder.  Fall back to the directory of the first
file, but only when it actually contains a Papis info file."
  (let ((folder (papis-document-get document "_papis_local_folder")))
    (cond
     ((and (stringp folder) (not (string-empty-p folder)))
      (file-name-as-directory (expand-file-name folder)))
     (t
      (let ((directory (papis-document-directory document)))
        (when (and directory
                   (file-exists-p
                    (expand-file-name papis-info-file-name directory)))
          directory))))))

(defun papis-document-url (document)
  "Return the most useful URL of DOCUMENT, or nil."
  (let ((url (papis-document-get document "url"))
        (doc-url (papis-document-get document "doc_url"))
        (doi (papis-document-get document "doi"))
        (eprint (papis-document-get document "eprint")))
    (cond
     ((and (stringp url) (not (string-empty-p url))) url)
     ((and (stringp doc-url) (not (string-empty-p doc-url))) doc-url)
     ((and (stringp doi) (not (string-empty-p doi)))
      (concat "https://doi.org/" doi))
     ((and (stringp eprint) (not (string-empty-p eprint)))
      (concat "https://arxiv.org/abs/" eprint))
     (t nil))))

(defun papis-document-description (document)
  "Return a short one-line description of DOCUMENT."
  (let ((author (papis-document-author document))
        (year (papis-document-year-string document)))
    (string-join (delq nil (list (unless (string-empty-p author) author)
                                 (unless (string-empty-p year) year)))
                 " · ")))

(defun papis-document-citation (document)
  "Return a human-readable citation of DOCUMENT."
  (let ((author (papis-document-author document))
        (year (papis-document-year-string document)))
    (format "%s%s. %s%s"
            (if (string-empty-p author) "Unknown author" author)
            (if (string-empty-p year) "" (format " (%s)" year))
            (papis-document-title document)
            ".")))

(defun papis-document-fields (document)
  "Return the metadata of DOCUMENT as an alist of (KEY . VALUE).

Keys are strings and are sorted according to
`papis-document-field-order'."
  (mapcar (lambda (key)
            (cons key (papis-document-get document key)))
          (papis--sorted-field-keys (papis-document-keys document))))

(defun papis--sorted-field-keys (keys)
  "Sort field KEYS according to `papis-document-field-order'."
  (let ((order papis-document-field-order))
    (sort (copy-sequence keys)
          (lambda (a b)
            (let ((ra (or (cl-position a order :test #'equal)
                          (length order)))
                  (rb (or (cl-position b order :test #'equal)
                          (length order))))
              (if (= ra rb)
                  (string< a b)
                (< ra rb)))))))

(defun papis-field-label (key)
  "Return a display label for metadata KEY."
  (let ((label (capitalize (replace-regexp-in-string "[-_]" " " key))))
    (pcase label
      ("Doi" "DOI")
      ("Isbn" "ISBN")
      ("Url" "URL")
      ("Papis id" "Papis ID")
      ("Doc url" "Doc URL")
      (_ label))))

;;; Queries

(defun papis--parse-documents (data library)
  "Turn parsed JSON DATA into a list of `papis-document' for LIBRARY."
  (unless (cl-every #'listp data)
    (signal 'papis-error
            (list "Unexpected document response from Papis")))
  (mapcar (lambda (item)
            (papis-document--create :data item :library library))
          data))

(defun papis--papis-program ()
  "Return the Papis executable to run."
  (or (and papis-executable
           (file-executable-p papis-executable)
           papis-executable)
      (executable-find "papis")
      (signal 'papis-error
              (list "Cannot find the `papis' executable; customize `papis-executable'"))))

(defun papis--call-cli-result (arguments)
  "Run `papis' with ARGUMENTS and return a plist.


The plist has the keys `:status', `:stdout' and `:stderr'.  No
error is signalled: Papis sometimes exits successfully without
doing anything, so callers may need to inspect the output."
  (let ((program (papis--papis-program))
        (stderr-file (make-temp-file "papis-stderr-")))
    (unwind-protect
        (with-temp-buffer
          (let ((status (apply #'call-process program nil
                               (list t stderr-file) nil arguments))
                (stdout (buffer-string))
                (stderr (with-temp-buffer
                          (insert-file-contents stderr-file)
                          (buffer-string))))
            (list :status status :stdout stdout :stderr stderr)))
      (delete-file stderr-file))))

(defun papis--call-cli (arguments)
  "Run `papis' with ARGUMENTS and return its standard output.

Signal `papis-error' when the process fails."
  (let* ((result (papis--call-cli-result arguments))
         (status (plist-get result :status))
         (output (string-trim (plist-get result :stdout)))
         (errors (string-trim (plist-get result :stderr))))
    (if (zerop status)
        (plist-get result :stdout)
      (signal 'papis-error
              (list (format "`papis %s' failed (exit %s): %s%s"
                            (string-join arguments " ")
                            status
                            output
                            (if (string-empty-p errors)
                                ""
                              (format "\n%s" errors))))))))

(defun papis--documents-via-python (library query)
  "Fetch documents matching QUERY in LIBRARY with the Python helper.

It runs `papis-dump-script' through `papis exec'."
  (unless (file-exists-p papis-dump-script)
    (signal 'papis-error
            (list (format "Papis dump script not found: %s"
                          papis-dump-script))))
  (let* ((output (papis--call-cli
                  (list "-l" library "exec" papis-dump-script
                        library query)))
         (line (seq-find (lambda (line)
                           (string-prefix-p "PAPIS_JSON:" line))
                         (split-string output "\n"))))
    (unless line
      (signal 'papis-error
              (list (format "Papis helper produced no JSON: %s"
                            (string-trim output)))))
    (let ((data (json-parse-string (substring line (length "PAPIS_JSON:"))
                                   :object-type 'alist
                                   :array-type 'list
                                   :null-object nil
                                   :false-object nil)))
      (papis--parse-documents data library))))

(defun papis-documents (&optional library query)
  "Return the documents of LIBRARY matching QUERY.

QUERY is a normal Papis query, such as \"author:knuth year:1984\".
When QUERY is nil or empty, all documents are returned.  LIBRARY
defaults to `papis-default-library' or the first Papis library."
  (let* ((library (or library (papis--default-library)))
         (query (or query "")))
    (unless library
      (signal 'papis-error
              (list "No Papis library found; configure one with `papis init' or Papis' config file")))
    (papis--documents-via-python library query)))

(defun papis-query (query &optional library)
  "Return the documents of LIBRARY matching QUERY."
  (papis-documents library query))

;;; Deleting documents

(defun papis-document-delete-arguments (document)
  "Return the `papis rm' arguments that delete DOCUMENT.

Prefer `--doc-folder', which identifies the document by its path
and works regardless of whether the database can find it by id."
  (let* ((library (or (papis-document-library document)
                      (papis--default-library)))
         (id (papis-document-id document))
         (folder (papis-document-folder document))
         (selector (cond
                    (folder (list "--doc-folder" folder))
                    (id (list (format "papis_id:%s" id)))
                    (t nil))))
    (unless library
      (signal 'papis-error
              (list "Cannot determine the library of the document")))
    (unless selector
      (signal 'papis-error
              (list "Cannot delete a document without an id or folder")))
    (append (list "-l" library "rm" "--force" "--all") selector)))

(defun papis-document-delete (document)
  "Delete DOCUMENT from its library.

This removes the document folder from disk and from the Papis
database, using the `papis rm' command.  No confirmation is
performed here; callers should ask the user first.

The folder is checked afterwards, because `papis rm' exits with
status 0 when its query matches nothing.  Signal `papis-error'
when the folder is still there."
  (let* ((arguments (papis-document-delete-arguments document))
         (folder (papis-document-folder document))
         (existed (and folder (file-directory-p folder)))
         (result (papis--call-cli-result arguments))
         (status (plist-get result :status))
         (output (string-trim
                  (concat (plist-get result :stdout)
                          (plist-get result :stderr)))))
    (when papis-debug
      (message "Papis: %s %s (exit %s)\n%s"
               (papis--papis-program)
               (string-join arguments " ")
               status output))
    (cond
     ;; The folder survived: `papis rm' matched nothing and the
     ;; document is still there.
     ((and existed (file-directory-p folder))
      (signal 'papis-error
              (list (format "`papis %s' did not delete %s%s"
                            (string-join arguments " ")
                            folder
                            (if (string-empty-p output)
                                ""
                              (format "\n%s" output))))))
     ;; The folder is gone but papis still reported a failure, e.g.
     ;; when it removed the folder and then failed to update the
     ;; database.  Deleting the files is what matters here.
     ((not (zerop status))
      (if existed
          (progn
            (display-warning
             'papis
             (format "Removed %s, but `papis %s' failed (exit %s): %s"
                     folder (string-join arguments " ") status output)
             :warning)
            t)
        (signal 'papis-error
                (list (format "`papis %s' failed (exit %s): %s"
                              (string-join arguments " ")
                              status output)))))
     ;; No folder to verify: trust the command, but not when Papis
     ;; said that nothing was matched.
     ((and (not existed) (string-match-p "No documents retrieved" output))
      (signal 'papis-error
              (list (format "`papis %s' matched no document"
                            (string-join arguments " ")))))
     (t t))))

;;; Tags

(defun papis-document-tag-arguments (document &rest actions)
  "Return the `papis tag' arguments that apply ACTIONS to DOCUMENT.

ACTIONS are appended to the command, for example
\(\"--add\" \"reading\"\).  The document is selected by its
folder when known, otherwise by its id."
  (let* ((library (or (papis-document-library document)
                      (papis--default-library)))
         (id (papis-document-id document))
         (folder (papis-document-folder document))
         (selector (cond
                    (folder (list "--doc-folder" folder))
                    (id (list (format "papis_id:%s" id)))
                    (t nil))))
    (unless library
      (signal 'papis-error
              (list "Cannot determine the library of the document")))
    (unless selector
      (signal 'papis-error
              (list "Cannot change the tags of a document without an id or folder")))
    (append (list "-l" library "tag" "--batch" "--all") selector actions)))

(defun papis--document-set-field (document key value)
  "Store VALUE for KEY in DOCUMENT's in-memory metadata.

This keeps the document in the frontend in sync with a command
that changed the metadata on disk; refresh the Papis cache so
other tools see the new value."
  (let* ((symbol (papis--document-key-symbol key))
         (cell (assoc symbol (papis-document-data document))))
    (if cell
        (setcdr cell value)
      (push (cons symbol value) (papis-document-data document)))
    value))

(defun papis--document-set-tags (document tags)
  "Store TAGS in DOCUMENT's in-memory metadata and return them.

This keeps the document in the frontend in sync with the tag
command; refresh the Papis cache so other tools see the new tags."
  (papis--document-set-field document "tags" tags))

(defun papis--document-tag-change (document action tag)
  "Apply tag ACTION, either \"add\" or \"remove\", for TAG to DOCUMENT."
  (let ((tag (string-trim (or tag ""))))
    (when (string-empty-p tag)
      (signal 'papis-error (list "A tag cannot be empty")))
    (let* ((arguments (apply #'papis-document-tag-arguments document
                             (list (format "--%s" action) tag)))
           (result (papis--call-cli-result arguments))
           (status (plist-get result :status))
           (output (string-trim
                    (concat (plist-get result :stdout)
                            (plist-get result :stderr)))))
      (when papis-debug
        (message "Papis: %s %s (exit %s)\n%s"
                 (papis--papis-program)
                 (string-join arguments " ")
                 status output))
      (unless (zerop status)
        (signal 'papis-error
                (list (format "`papis %s' failed (exit %s): %s"
                              (string-join arguments " ")
                              status
                              (if (string-empty-p output)
                                  "no output"
                                output)))))
      (papis--document-set-tags
       document
       (let ((tags (papis-document-tags document)))
         (pcase action
           ("add" (if (member tag tags) tags (append tags (list tag))))
           ("remove" (seq-remove (lambda (candidate)
                                    (equal candidate tag))
                                  tags))
           (_ tags))))
      (papis-document-refresh-cache document)
      t)))

(defun papis-document-tag-add (document tag)
  "Add TAG to DOCUMENT with `papis tag --add'.

An existing tag is left alone, so adding the same tag twice is
harmless.  Return non-nil on success."
  (papis--document-tag-change document "add" tag))

(defun papis-document-tag-remove (document tag)
  "Remove TAG from DOCUMENT with `papis tag --remove'.

Signal `papis-error' when Papis reports that the tag is not there.
Return non-nil on success."
  (papis--document-tag-change document "remove" tag))

;;; Metadata

(defun papis-document-field-name (key)
  "Return KEY as a lowercase Papis metadata field name."
  (downcase
   (cond
    ((keywordp key) (substring (symbol-name key) 1))
    ((symbolp key) (symbol-name key))
    (t (format "%s" key)))))

(defun papis--document-update-fields (fields)
  "Return FIELDS without entries whose value is nil or empty."
  (seq-filter
   (lambda (field)
     (not (string-empty-p
           (string-trim (papis--value-to-string (cdr field))))))
   fields))

(defun papis--document-selector-arguments (document)
  "Return (LIBRARY . ARGUMENTS) that select DOCUMENT for a CLI command.

The document is selected by its folder when known, otherwise by
its id."
  (let* ((library (or (papis-document-library document)
                      (papis--default-library)))
         (id (papis-document-id document))
         (folder (papis-document-folder document))
         (selector (cond
                    (folder (list "--doc-folder" folder))
                    (id (list (format "papis_id:%s" id)))
                    (t nil))))
    (unless library
      (signal 'papis-error
              (list "Cannot determine the library of the document")))
    (unless selector
      (signal 'papis-error
              (list "Cannot update a document without an id or folder")))
    (cons library selector)))

(defun papis-document-update-arguments (document fields)
  "Return the `papis update' arguments that set FIELDS on DOCUMENT.

FIELDS is an alist of (KEY . VALUE).  The document is selected by
its folder when known, otherwise by its id."
  (let* ((selection (papis--document-selector-arguments document))
         (settings
          (seq-mapcat
           (lambda (field)
             (list "--set"
                   (papis-document-field-name (car field))
                   (papis--value-to-string (cdr field))))
           fields)))
    (append (list "-l" (car selection) "update" "--batch" "--all")
            (cdr selection) settings)))

(defun papis-document-add-file-arguments (document file)
  "Return the `papis update' arguments that append FILE to DOCUMENT."
  (let ((selection (papis--document-selector-arguments document)))
    (append (list "-l" (car selection) "update" "--batch" "--all")
            (cdr selection)
            (list "--append" "files" file))))

(defun papis-document-add-file (document file)
  "Attach FILE to DOCUMENT with `papis update --append files'.

FILE should be relative to the document folder, as Papis stores
it.  Update DOCUMENT's in-memory file list and return FILE."
  (let* ((arguments (papis-document-add-file-arguments document file))
         (result (papis--call-cli-result arguments))
         (status (plist-get result :status))
         (output (string-trim
                  (concat (plist-get result :stdout)
                          (plist-get result :stderr)))))
    (when papis-debug
      (message "Papis: %s %s (exit %s)\n%s"
               (papis--papis-program)
               (string-join arguments " ")
               status output))
    (unless (zerop status)
      (signal 'papis-error
              (list (format "`papis %s' failed (exit %s): %s"
                            (string-join arguments " ")
                            status
                            (if (string-empty-p output)
                                "no output"
                              output)))))
    (papis--document-set-field
     document "files"
     (append (papis-document-files document) (list file)))
    (papis-document-refresh-cache document)
    file))

(defun papis-document-update (document fields)
  "Set metadata FIELDS on DOCUMENT with `papis update --set'.

FIELDS is an alist of (KEY . VALUE); entries with a nil or empty
value are ignored.  Update DOCUMENT's in-memory metadata as well
and return the fields that were applied.  The Papis cache is
refreshed so other tools see the new metadata."
  (let ((fields (papis--document-update-fields fields)))
    (unless fields
      (user-error "No metadata fields to update"))
    (let* ((arguments (papis-document-update-arguments document fields))
           (result (papis--call-cli-result arguments))
           (status (plist-get result :status))
           (output (string-trim
                    (concat (plist-get result :stdout)
                            (plist-get result :stderr)))))
      (when papis-debug
        (message "Papis: %s %s (exit %s)\n%s"
                 (papis--papis-program)
                 (string-join arguments " ")
                 status output))
      (unless (zerop status)
        (signal 'papis-error
                (list (format "`papis %s' failed (exit %s): %s"
                              (string-join arguments " ")
                              status
                              (if (string-empty-p output)
                                  "no output"
                                output)))))
      (dolist (field fields)
        (papis--document-set-field document (car field) (cdr field)))
      (papis-document-refresh-cache document)
      fields)))

;;; Book metadata

(defun papis-document-book-files (document)
  "Return the EPUB and PDF files attached to DOCUMENT as absolute names."
  (let ((directory (or (papis-document-folder document)
                       (papis-document-directory document)
                       default-directory)))
    (seq-filter
     (lambda (file)
       (string-match-p "\\.\\(?:epub\\|pdf\\)\\'" (downcase file)))
     (mapcar (lambda (file)
               (if (file-name-absolute-p file)
                   file
                 (expand-file-name file directory)))
             (papis-document-files document)))))

(defun papis-document--existing-book-files (document)
  "Return EPUB and PDF files of DOCUMENT that exist on disk.

Files recorded in DOCUMENT come first; EPUB/PDF files found in its
folder are added as well.  The folder scan recovers from a stale
`files' value in the Papis cache, for example after the book file
was renamed but the cache was not refreshed."
  (let* ((folder (papis-document-folder document))
         (recorded (seq-filter #'file-exists-p
                               (papis-document-book-files document)))
         (scanned
          (when folder
            (seq-filter
             (lambda (file)
               (and (file-regular-p file)
                    (string-match-p "\\.\\(?:epub\\|pdf\\)\\'"
                                    (downcase file))))
             (directory-files folder t)))))
    (delete-dups (append recorded scanned))))

(defun papis-document-epub-file (document)
  "Return the EPUB file attached to DOCUMENT, or nil.

When the recorded EPUB no longer exists, fall back to an EPUB found
in the document folder."
  (seq-find (lambda (file)
              (string-match-p "\\.epub\\'" (downcase file)))
            (papis-document--existing-book-files document)))

(defun papis--book-metadata-script-command (&rest arguments)
  "Return a command list running the metadata script with ARGUMENTS.

The first element is the program and the rest are its arguments.
The script is executed directly when it is executable (its shebang
runs `uv'), otherwise it is run through `uv run --script'."
  (let ((script (expand-file-name papis-book-metadata-script)))
    (unless (file-exists-p script)
      (signal 'papis-error
              (list (format "Book metadata script not found: %s" script))))
    (cond
     ((file-executable-p script)
      (cons script arguments))
     ((executable-find "uv")
      (append (list (executable-find "uv") "run" "--script" script)
              arguments))
     (t
      (signal 'papis-error
              (list (format "Cannot run %s; make it executable or install `uv'"
                            script)))))))

(defun papis-document-book-metadata-command (file)
  "Return the command list that runs the metadata script on FILE."
  (papis--book-metadata-script-command file))

(defun papis-document-embed-cover-command (file cover)
  "Return the command list that embeds COVER into EPUB FILE."
  (papis--book-metadata-script-command "--embed-cover" cover file))

(defun papis--run-book-metadata-script (file)
  "Run the book metadata script on FILE and return its output.

Signal `papis-error' when the script exits unsuccessfully."
  (papis--run-metadata-command (papis-document-book-metadata-command file)))

(defun papis--run-metadata-command (command)
  "Run metadata script COMMAND and return its combined output.

Signal `papis-error' when the script exits unsuccessfully."
  (let* ((stderr-file (make-temp-file "papis-book-metadata-stderr-"))
         status stdout stderr)
    (unwind-protect
        (with-temp-buffer
          (setq status (apply #'call-process (car command) nil
                              (list t stderr-file) nil (cdr command))
                stdout (buffer-string)
                stderr (with-temp-buffer
                         (insert-file-contents stderr-file)
                         (buffer-string))))
      (delete-file stderr-file))
    (let ((stdout (string-trim stdout))
          (stderr (string-trim stderr)))
      (unless (zerop status)
        (signal 'papis-error
                (list (format "Book metadata update failed (exit %s): %s%s"
                              status
                              stdout
                              (if (string-empty-p stderr)
                                  ""
                                (format "\n%s" stderr))))))
      (string-join (delq nil (list (unless (string-empty-p stdout) stdout)
                                   (unless (string-empty-p stderr) stderr)))
                   "\n"))))

(defun papis-document-refresh-cache (document)
  "Reload DOCUMENT's metadata in the Papis cache.

Changes made to `info.yaml' by an external program are not visible
to Papis until the cache is updated.  Rebuild only DOCUMENT's entry
when its folder is known, otherwise update all newer entries in the
library."
  (let* ((library (or (papis-document-library document)
                      (papis--default-library)))
         (folder (papis-document-folder document)))
    (unless library
      (signal 'papis-error
              (list "Cannot determine the library of the document")))
    (let* ((arguments (append (list "-l" library "cache"
                                   (if folder "update" "update-newer")
                                   "--all")
                             (when folder (list "--doc-folder" folder))))
           (result (papis--call-cli-result arguments))
           (status (plist-get result :status))
           (output (string-trim
                    (concat (plist-get result :stdout)
                            (plist-get result :stderr)))))
      (when papis-debug
        (message "Papis: %s %s (exit %s)\n%s"
                 (papis--papis-program)
                 (string-join arguments " ")
                 status output))
      (unless (zerop status)
        (display-warning
         'papis
         (format "Updated the metadata, but `papis %s' failed (exit %s): %s"
                 (string-join arguments " ") status output)
         :warning))
      result)))

(defun papis-document-update-book-metadata (document &optional file)
  "Update DOCUMENT's book metadata and rename its EPUB or PDF file.

Run `papis-book-metadata-script' on FILE, which defaults to the
first EPUB or PDF attached to DOCUMENT.  The script merges the
file's metadata with `info.yaml', writes both files back and
renames the file to \"author - title (year).epub\" (or `.pdf').
PDF metadata is written with ExifTool.  Refresh the Papis cache
afterwards so Papis returns the new metadata.  Return the script
output."
  (let ((file (or file (car (papis-document--existing-book-files document)))))
    (unless file
      (user-error "Document \"%s\" has no EPUB or PDF file"
                  (papis-document-title document)))
    (unless (file-exists-p file)
      (signal 'papis-error (list (format "Book file not found: %s" file))))
    (unless (string-match-p "\\.\\(?:epub\\|pdf\\)\\'" (downcase file))
      (signal 'papis-error
              (list (format "Not an EPUB or PDF file: %s" file))))
    (let ((output (papis--run-book-metadata-script file)))
      (papis-document-refresh-cache document)
      output)))

(defun papis-document-embed-cover (document cover)
  "Embed the image COVER into DOCUMENT's EPUB file.

Return the script output.  Signal `user-error' when DOCUMENT has no
EPUB file and `papis-error' when the script fails."
  (let ((file (papis-document-epub-file document)))
    (unless file
      (user-error "Document \"%s\" has no EPUB file"
                  (papis-document-title document)))
    (unless (file-exists-p file)
      (signal 'papis-error (list (format "Book file not found: %s" file))))
    (papis--run-metadata-command
     (papis-document-embed-cover-command file cover))))

(defun papis-document-sync-metadata (document)
  "Sync DOCUMENT's metadata after `info.yaml' was edited.

When DOCUMENT has an EPUB or PDF file, run the book metadata script
so the file is updated and renamed.  Otherwise only refresh the
Papis cache."
  (if (papis-document--existing-book-files document)
      (papis-document-update-book-metadata document)
    (papis-document-refresh-cache document)))

(defun papis-read-document (&optional prompt)
  "Prompt for a library and one of its documents and return it.
PROMPT is used as the library prompt."
  (let* ((library (completing-read (or prompt "Library: ")
                                   (papis-libraries) nil t))
         (documents (papis-documents library ""))
         (choices (mapcar (lambda (document)
                            (let ((author (papis-document-author document))
                                  (year (papis-document-year-string document)))
                              (cons (format "%s%s"
                                            (papis-document-title document)
                                            (cond
                                             ((not (string-empty-p author))
                                              (format " — %s" author))
                                             ((not (string-empty-p year))
                                              (format " (%s)" year))
                                             (t "")))
                                    document)))
                          documents)))
    (unless choices
      (user-error "No documents in library \"%s\"" library))
    (cdr (assoc (completing-read "Document: " choices nil t) choices))))

(defun papis-update-book-metadata (&optional document)
  "Update DOCUMENT's EPUB or PDF metadata with the metadata script.

Interactively, prompt for a library and a document.  The script
merges `info.yaml' with the file's metadata, writes both back and
renames the file, then refreshes the Papis cache.  Return the script
output."
  (interactive)
  (let* ((document (or document (papis-read-document)))
         (output (papis-document-update-book-metadata document)))
    (when (and (called-interactively-p 'interactive)
               (not (string-empty-p (string-trim output))))
      (message "%s" (string-trim output)))
    output))

;;; Export

(defun papis-document-format (library query format)
  "Export documents matching QUERY in LIBRARY using Papis exporter FORMAT.

FORMAT is a Papis exporter name, such as \"bibtex\" or \"yaml\".
Return the exported string."
  (let ((query (string-trim (or query ""))))
    (papis--clean-cli-output
     (papis--call-cli
      (append (list "-l" library "export" "--batch" "--all"
                    "-f" format)
              (unless (string-empty-p query) (list query)))))))

(defun papis--clean-cli-output (text)
  "Remove Papis warnings and log lines from CLI output TEXT."
  (string-trim
   (mapconcat #'identity
              (seq-remove
               (lambda (line)
                 (or (string-match-p "\\`The configuration is loaded from" line)
                     (string-match-p "\\`A deprecated configuration folder" line)
                     (string-match-p "\\`\\[[A-Z]+\\]" line)))
               (split-string text "\n"))
              "\n")))

(defun papis--export-via-cli (document format)
  "Export DOCUMENT with FORMAT using the Papis CLI."
  (let* ((library (or (papis-document-library document)
                      (papis--default-library)))
         (id (papis-document-id document))
         (directory (papis-document-directory document))
         (selector (cond
                    (id (list (format "papis_id:%s" id)))
                    (directory (list "--doc-folder" directory))
                    (t nil))))
    (unless selector
      (signal 'papis-error
              (list "Cannot export a document without an id or folder")))
    (papis--clean-cli-output
     (papis--call-cli
      (append (list "-l" library "export" "--batch" "-f" format)
              selector)))))

(defun papis-document-export (document format)
  "Export DOCUMENT using Papis exporter FORMAT."
  (papis--export-via-cli document format))

;;; Opening documents

(defun papis-document-open-file (document)
  "Open the first file of DOCUMENT with `papis-open-file-function'."
  (let ((file (papis-document-file document)))
    (unless file
      (user-error "Document \"%s\" has no files" (papis-document-title document)))
    (funcall papis-open-file-function file)))

(defun papis-document-open-url (document)
  "Open the URL of DOCUMENT in a browser."
  (let ((url (papis-document-url document)))
    (unless url
      (user-error "Document \"%s\" has no URL" (papis-document-title document)))
    (browse-url url)))

(defun papis-document-open-directory (document)
  "Open the folder of DOCUMENT with Dired."
  (let ((directory (papis-document-directory document)))
    (unless directory
      (user-error "Cannot determine the folder of \"%s\""
                  (papis-document-title document)))
    (dired directory)))

(defun papis-document-open-info (document)
  "Open the info file of DOCUMENT."
  (let ((file (papis-document-info-file document)))
    (unless (and file (file-exists-p file))
      (user-error "Document \"%s\" has no info file"
                  (papis-document-title document)))
    (find-file file)))

(provide 'papis)
;;; papis.el ends here
