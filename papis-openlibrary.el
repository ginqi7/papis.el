;;; papis-openlibrary.el --- Open Library metadata for Papis  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/ginqi7/papis.el
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

;; Optional Open Library integration for papis.el.  It searches Open
;; Library by ISBN or by title and author, turns a search result into
;; Papis metadata fields (title, author, year, publisher and ISBN) and
;; can download a cover into the document folder (and embed it in the
;; document's EPUB).
;;
;; This module is used by the `m' command of the ETAF gallery, but its
;; functions are also useful on their own:
;;
;;   (papis-openlibrary-metadata-for-document document)
;;   (papis-openlibrary-set-cover document)
;;
;; ask for a matching Open Library entry and return the fields to
;; write or the cover file to attach; `papis-openlibrary-search',
;; `papis-openlibrary-metadata' and `papis-openlibrary-cover-spec' work
;; on plain data without prompting.
;;
;; It needs openlibrary.el (and its plz.el dependency) on `load-path';
;; the library is loaded on demand, so papis.el and the gallery keep
;; working without it.

;;; Code:

(require 'cl-lib)
(require 'image)
(require 'seq)
(require 'subr-x)
(require 'papis)

(declare-function openlibrary-search "openlibrary"
                  (query &rest arguments))
(declare-function openlibrary-cover-url "openlibrary"
                  (id &rest arguments))
(declare-function openlibrary-download-cover "openlibrary"
                  (id filename &rest arguments))

(defgroup papis-openlibrary nil
  "Open Library metadata lookup for Papis."
  :group 'papis
  :prefix "papis-openlibrary-")

(defcustom papis-openlibrary-limit 10
  "Number of Open Library search results offered."
  :type 'integer
  :group 'papis-openlibrary)

(defcustom papis-openlibrary-cover-file-name "cover.jpg"
  "File name used when saving an Open Library cover in a document folder."
  :type 'string
  :group 'papis-openlibrary)

(defconst papis-openlibrary--fields
  '(key title author_name first_publish_year publisher isbn
    cover_i cover_edition_key)
  "Open Library search fields requested for metadata and cover lookup.")

(defun papis-openlibrary--string (value)
  "Return VALUE as a non-empty string, or nil."
  (let ((string (string-trim (papis--value-to-string value))))
    (unless (string-empty-p string) string)))

(defun papis-openlibrary-search (query &optional isbn)
  "Return Open Library search results for QUERY or ISBN.

When ISBN is non-nil it is used as the search key and QUERY is
ignored.  Signal a `user-error' when openlibrary.el is missing or
the request fails."
  (unless (require 'openlibrary nil t)
    (user-error "openlibrary.el is not available; add it to `load-path'"))
  (let ((result
         (condition-case err
             (if isbn
                 (openlibrary-search "" :isbn isbn
                                     :limit papis-openlibrary-limit
                                     :fields papis-openlibrary--fields)
               (openlibrary-search query
                                   :limit papis-openlibrary-limit
                                   :fields papis-openlibrary--fields))
           (error
            (user-error "Open Library request failed: %s"
                        (error-message-string err))))))
    (alist-get "docs" result nil nil #'equal)))

(defun papis-openlibrary-label (doc)
  "Return the completion label of Open Library result DOC."
  (let ((title (or (papis-openlibrary--string
                    (alist-get "title" doc nil nil #'equal))
                   "Untitled"))
        (authors (delq nil
                       (mapcar #'papis-openlibrary--string
                               (alist-get "author_name" doc nil nil #'equal))))
        (year (papis-openlibrary--string
               (alist-get "first_publish_year" doc nil nil #'equal))))
    (format "%s — %s%s"
            title
            (if authors (string-join authors ", ") "Unknown author")
            (if year (format " (%s)" year) ""))))

(defun papis-openlibrary-isbn (isbns)
  "Return the ISBN to use from ISBNS, preferring a 13-digit one."
  (let ((isbns (delq nil
                     (mapcar #'papis-openlibrary--string
                             (if (listp isbns) isbns (list isbns))))))
    (or (seq-find (lambda (isbn)
                    (= 13 (length (replace-regexp-in-string
                                   "[^0-9Xx]" "" isbn))))
                  isbns)
        (car isbns))))

(defun papis-openlibrary-metadata (doc)
  "Return Papis metadata fields built from Open Library result DOC."
  (let ((title (alist-get "title" doc nil nil #'equal))
        (authors (alist-get "author_name" doc nil nil #'equal))
        (year (alist-get "first_publish_year" doc nil nil #'equal))
        (publishers (alist-get "publisher" doc nil nil #'equal))
        (isbns (alist-get "isbn" doc nil nil #'equal)))
    (delq nil
          (list (cons "title" (papis-openlibrary--string title))
                (cons "author"
                      (and authors
                           (string-join
                            (delq nil
                                  (mapcar #'papis-openlibrary--string
                                          authors))
                            " and ")))
                (cons "year" (papis-openlibrary--string year))
                (cons "publisher"
                      (papis-openlibrary--string
                       (if (listp publishers) (car publishers) publishers)))
                (cons "isbn" (papis-openlibrary-isbn isbns))))))

(defun papis-openlibrary-query (document)
  "Return the Open Library query string for DOCUMENT."
  (string-trim
   (concat (papis-document-title document) " "
           (papis-document-author document))))

(defun papis-openlibrary-read-result (document)
  "Search Open Library for DOCUMENT and return the chosen search result.

Search by ISBN when DOCUMENT has one, otherwise by title and
author, and let the user pick one of the matches.  Signal a
`user-error' when no result is found."
  (let* ((isbn (papis-document-isbn document))
         (query (papis-openlibrary-query document))
         (docs (progn
                 (message "Searching Open Library…")
                 (papis-openlibrary-search query isbn))))
    (unless docs
      (setq query (read-string "Open Library query: " query)
            docs (papis-openlibrary-search query)))
    (unless docs
      (user-error "No Open Library match for \"%s\"" query))
    (let* ((choices (mapcar (lambda (doc)
                              (cons (papis-openlibrary-label doc) doc))
                            docs))
           (label (completing-read "Open Library: " choices nil t)))
      (cdr (assoc label choices)))))

(defun papis-openlibrary-metadata-for-document (document)
  "Ask Open Library for metadata of DOCUMENT and return chosen fields.

Let the user pick a search result and confirm the update.  Return
nil when the user declines.  Signal a `user-error' when no result
is found."
  (let* ((chosen (papis-openlibrary-read-result document))
         (fields (papis-openlibrary-metadata chosen)))
    (unless fields
      (user-error "Open Library returned no usable metadata"))
    (when (y-or-n-p
           (format "Update \"%s\" with:\n%s\n"
                   (papis-document-title document)
                   (string-join
                    (mapcar (lambda (field)
                              (format "%s: %s" (car field) (cdr field)))
                            fields)
                    "\n")))
      fields)))

;;; Covers

(defun papis-openlibrary-cover-spec (result &optional document)
  "Return (ID . TYPE) describing a cover for Open Library RESULT.

TYPE is `id' for a cover id from RESULT, `isbn' for DOCUMENT's ISBN,
or `olid' for the result's cover edition.  Return nil when no
candidate is available."
  (let ((cover-id (alist-get "cover_i" result nil nil #'equal))
        (edition (alist-get "cover_edition_key" result nil nil #'equal))
        (isbn (and document (papis-document-isbn document))))
    (cond
     ((and cover-id
           (not (equal cover-id 0))
           (not (equal cover-id "0"))
           (not (and (stringp cover-id) (string-empty-p cover-id))))
      (cons cover-id 'id))
     ((and isbn (not (string-empty-p isbn)))
      (cons isbn 'isbn))
     ((and edition (stringp edition) (not (string-empty-p edition)))
      (cons edition 'olid)))))

(defun papis-openlibrary-cover-url (result &optional document)
  "Return the URL of a cover for Open Library RESULT, or nil."
  (when-let* ((spec (papis-openlibrary-cover-spec result document)))
    (unless (require 'openlibrary nil t)
      (user-error "openlibrary.el is not available; add it to `load-path'"))
    (openlibrary-cover-url (car spec) :type (cdr spec) :size 'L)))

(defun papis-openlibrary-download-cover (result directory &optional document)
  "Download the cover of Open Library RESULT into DIRECTORY.

The image is saved as `papis-openlibrary-cover-file-name'.  Return
the file name, or nil when the result has no usable cover."
  (when-let* ((spec (papis-openlibrary-cover-spec result document)))
    (unless (require 'openlibrary nil t)
      (user-error "openlibrary.el is not available; add it to `load-path'"))
    (let* ((temporary (make-temp-file "papis-cover-" nil ".jpg"))
           (destination (expand-file-name papis-openlibrary-cover-file-name
                                          directory)))
      (unwind-protect
          (progn
            ;; `openlibrary-download-cover' requires a name that does not exist.
            (delete-file temporary)
            (condition-case nil
                (openlibrary-download-cover (car spec) temporary
                                            :type (cdr spec) :size 'L)
              (error nil))
            (when (and (file-exists-p temporary)
                       (> (or (file-attribute-size
                               (file-attributes temporary))
                              0)
                          0)
                       (image-type-from-file-header temporary))
              (rename-file temporary destination t)
              destination))
        (when (file-exists-p temporary)
          (ignore-errors (delete-file temporary)))))))

(defun papis-openlibrary-set-cover (&optional document)
  "Search Open Library for a cover of DOCUMENT and attach it.

Interactively, prompt for a library and document.  The chosen cover
is saved in the document folder and appended to the document's
`files' metadata.  When the document has an EPUB file, the image is
also written into the EPUB as its cover.  Return the cover file,
or nil."
  (interactive)
  (let* ((document (or document (papis-read-document)))
         (folder (papis-document-folder document))
         (destination (and folder
                           (expand-file-name
                            papis-openlibrary-cover-file-name folder)))
         (result nil))
    (unless folder
      (user-error "Cannot determine the folder of \"%s\""
                  (papis-document-title document)))
    (when (and (file-exists-p destination)
               (not (y-or-n-p
                     (format "Replace existing %s? "
                             papis-openlibrary-cover-file-name))))
      (user-error "Cover update cancelled"))
    (setq result (papis-openlibrary-read-result document))
    (let ((cover (papis-openlibrary-download-cover result folder document)))
      (unless cover
        (user-error "No cover available for \"%s\""
                    (papis-openlibrary-label result)))
      (unless (member (file-name-nondirectory cover)
                      (mapcar #'file-name-nondirectory
                              (papis-document-files document)))
        (papis-document-add-file document
                                 (file-name-nondirectory cover)))
      (when (papis-document-epub-file document)
        (papis-document-embed-cover document cover))
      cover)))

(provide 'papis-openlibrary)
;;; papis-openlibrary.el ends here
