;;; papis.el --- Talk to the Papis API  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Version: 0.1.0
;; URL: https://github.com/ginqi7/papis.el
;; Package-Requires: ((emacs "29.1") (vui "1.4.0"))
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
;; https://github.com/papis/papis.  It talks to the JSON API that
;; `papis serve' exposes:
;;
;;   GET /api/library
;;       -> ["books", "papers"]
;;
;;   GET /api/library/<library>
;;       -> {"name": "books", "path": "/home/me/Documents/books"}
;;
;;   GET /api/library/<library>/document
;;       -> [{"title": "...", "author": "...", ...}, ...]
;;
;;   GET /api/library/<library>/document/<query>
;;       -> documents matching a Papis query, e.g. "author:knuth"
;;
;;   GET /api/library/<library>/document/<query>/format/<exporter>
;;       -> the result of `papis export' as a JSON string
;;
;; All functions in this file return plain Lisp data.  Nothing here
;; knows about buffers, faces or VUI; that is `papis-vui.el'.
;;
;; The server is started on demand with `papis-start-server', which
;; runs `papis serve' as a child process.  If a server is already
;; listening on `papis-server-port' it is reused.
;;
;; A note on Papis 0.16: the server can fail to serialize documents
;; whose YAML `date' field was parsed as a date object.  In that case
;; the API returns an error ("Object of type date is not JSON
;; serializable").  When `papis-fallback-to-cli' is non-nil (the
;; default), `papis-documents' works around this: it first runs the
;; small `scripts/papis_dump.py' helper through `papis exec' (using Papis'
;; Python API with `default=str' JSON serialization), and if that is
;; unavailable, lists the matching ids and fetches each document
;; through the HTTP API by id.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url)

(defgroup papis nil
  "Emacs frontend for Papis."
  :group 'applications
  :prefix "papis-")

;;; Errors

(define-error 'papis-error "Papis error")
(define-error 'papis-http-error "Papis HTTP error" 'papis-error)
(define-error 'papis-server-error "Papis server error" 'papis-error)

(defun papis-error-message (error)
  "Return a clean message string for the error value ERROR.

Custom errors carry their message as the first data element, which
`error-message-string' would print with surrounding quotes."
  (let ((data (cdr-safe error)))
    (if (and data
             (stringp (car data))
             (memq (car-safe error)
                   '(papis-error papis-http-error papis-server-error)))
        (car data)
      (error-message-string error))))

;;; Custom variables

(defcustom papis-executable (or (executable-find "papis") "papis")
  "The Papis executable used to start the server."
  :type 'string
  :group 'papis)

(defcustom papis-server-host "127.0.0.1"
  "Host the Papis server listens on."
  :type 'string
  :group 'papis)

(defcustom papis-server-port 8888
  "Port the Papis server listens on.

This is also the port Papis uses by default (`papis serve -p 8888')."
  :type 'integer
  :group 'papis)

(defcustom papis-server-auto-start t
  "Whether API calls may start the Papis server automatically.

When nil, `papis-ensure-server' signals an error instead of running
`papis serve'.  A server started by another program is always reused."
  :type 'boolean
  :group 'papis)

(defcustom papis-server-start-timeout 10
  "Seconds to wait for `papis serve' to become responsive."
  :type 'number
  :group 'papis)

(defcustom papis-request-timeout 10
  "Seconds to wait for one HTTP request to the Papis server."
  :type 'number
  :group 'papis)

(defcustom papis-debug nil
  "Whether to log Papis HTTP requests in the echo area."
  :type 'boolean
  :group 'papis)

(defcustom papis-server-probe-timeout 0.5
  "Seconds to wait when checking whether the Papis server is alive."
  :type 'number
  :group 'papis)

(defcustom papis-server-log-buffer "*papis-server*"
  "Buffer receiving the output of the Papis server process."
  :type 'string
  :group 'papis)

(defcustom papis-default-library nil
  "Library used when none is given explicitly.

When nil, the value of the PAPIS_LIB environment variable and then
the first library reported by Papis are used."
  :type '(choice (const :tag "Automatic" nil) string)
  :group 'papis)

(defcustom papis-fallback-to-cli t
  "Whether to work around server JSON serialization errors.

Papis 0.16 fails to serialize documents with a date object in their
metadata.  When this is non-nil, `papis-documents' first tries
`scripts/papis_dump.py' through `papis exec' (Papis' Python API, with dates
serialized as strings).  If that is unavailable, it enumerates the
matching ids with `papis list --id' and reads each document through
the HTTP API by id."
  :type 'boolean
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
  "Python helper used when the Papis server cannot serialize documents.

It is run with `papis exec' so that it sees the Python environment
Papis is installed in.  See `papis-fallback-to-cli'."
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

;;; Internal state

(defvar papis--server-process nil
  "The `papis serve' process started by this Emacs, if any.")

;;; Server

(defun papis-server-url (&optional path)
  "Return the base URL of the Papis server, with optional PATH appended."
  (let ((host (if (string-match-p ":" papis-server-host)
                  (format "[%s]" papis-server-host)
                papis-server-host)))
    (concat (format "http://%s:%d" host papis-server-port)
            (or path ""))))

(defun papis--server-log-tail ()
  "Return the tail of the server log, prefixed with a newline."
  (let ((buffer (get-buffer papis-server-log-buffer)))
    (if (not (buffer-live-p buffer))
        ""
      (with-current-buffer buffer
        (let ((text (string-trim (buffer-string))))
          (if (string-empty-p text)
              ""
            (format "\n%s"
                    (substring text (max 0 (- (length text) 1000))))))))))

(defun papis-server-running-p ()
  "Return non-nil when a responsive Papis server is listening."
  (condition-case nil
      (let ((data (papis--http-get-json (papis-server-url "/api/library")
                                        papis-server-probe-timeout)))
        (and (listp data) t))
    (error nil)))

(defun papis-start-server ()
  "Start `papis serve' and wait until it answers requests.

If a server is already listening on `papis-server-port', reuse it.
Signal `papis-server-error' when the server cannot be started."
  (interactive)
  (if (papis-server-running-p)
      (progn
        (message "Papis server is already running at %s" (papis-server-url))
        t)
    (when (process-live-p papis--server-process)
      (delete-process papis--server-process)
      (setq papis--server-process nil))
    (let* ((program (or (and papis-executable
                             (file-executable-p papis-executable)
                             papis-executable)
                        (executable-find "papis")))
           (log-buffer (get-buffer-create papis-server-log-buffer))
           (process nil)
           (ready nil))
      (unless program
        (signal 'papis-server-error
                (list "Cannot find the `papis' executable; customize `papis-executable'")))
      (with-current-buffer log-buffer
        (erase-buffer))
      (setq process
            (make-process
             :name "papis-server"
             :buffer log-buffer
             :command (list program
                            "serve"
                            "--host" papis-server-host
                            "-p" (number-to-string papis-server-port))
             :connection-type 'pipe
             :noquery t
             :sentinel (lambda (proc _event)
                         (unless (process-live-p proc)
                           (when (eq proc papis--server-process)
                             (setq papis--server-process nil))))))
      (setq papis--server-process process)
      (let ((deadline (+ (float-time) papis-server-start-timeout)))
        (while (and (not ready)
                    (< (float-time) deadline)
                    (process-live-p process))
          (accept-process-output process 0.1)
          (setq ready (papis-server-running-p))))
      (cond
       (ready
        (message "Papis server started at %s" (papis-server-url))
        t)
       (t
        (when (process-live-p process)
          (delete-process process))
        (setq papis--server-process nil)
        (signal 'papis-server-error
                (list (format "Could not start the Papis server at %s%s"
                              (papis-server-url)
                              (papis--server-log-tail)))))))))

(defun papis-stop-server ()
  "Stop the Papis server started by this Emacs."
  (interactive)
  (if (process-live-p papis--server-process)
      (progn
        (delete-process papis--server-process)
        (setq papis--server-process nil)
        (message "Papis server stopped."))
    (message "No Papis server was started by this Emacs.")))

(defun papis-restart-server ()
  "Restart the Papis server managed by this Emacs."
  (interactive)
  (papis-stop-server)
  (papis-start-server))

(defun papis-ensure-server ()
  "Make sure a responsive Papis server is available.

Reuse a server already listening on `papis-server-port'.  Otherwise,
start one when `papis-server-auto-start' is non-nil."
  (cond
   ((papis-server-running-p) t)
   (papis-server-auto-start (papis-start-server))
   (t
    (signal 'papis-server-error
            (list (format "No Papis server at %s; run `papis-start-server' or set `papis-server-auto-start'"
                          (papis-server-url)))))))

(defun papis-open-server-log ()
  "Display the output of the Papis server process."
  (interactive)
  (pop-to-buffer (get-buffer-create papis-server-log-buffer)))

(defun papis--kill-emacs ()
  "Stop the Papis server child process when Emacs exits."
  (when (process-live-p papis--server-process)
    (delete-process papis--server-process)))

(add-hook 'kill-emacs-hook #'papis--kill-emacs)

;;; HTTP

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
              (format "server URL: %s\n" (papis-server-url))
              (format "server process: %s\n"
                      (if (process-live-p papis--server-process)
                          "live"
                        "not started by this Emacs"))
              (format "server responding: %s\n" (papis-server-running-p))
              (format "request timeout: %s\n" papis-request-timeout)
              (format "proxy services: %S\n" url-proxy-services)
              "\nLibraries:\n")
      (condition-case err
          (insert (format "%S\n" (papis-libraries)))
        (error
         (insert (format "ERROR: %s\n" (papis-error-message err)))))
      (insert "\nServer log:\n" (papis--server-log-tail) "\n")
      (special-mode)
      (goto-char (point-min)))
    (display-buffer buffer)))

(defun papis--error-message (body)
  "Extract a readable error message from the HTTP response BODY."
  (let ((body (string-trim (or body ""))))
    (if (string-empty-p body)
        "empty response"
      (condition-case nil
          (let ((data (json-parse-string body
                                         :object-type 'alist
                                         :array-type 'list
                                         :null-object nil
                                         :false-object nil)))
            (or (and (listp data)
                     (alist-get 'message data)
                     (format "%s" (alist-get 'message data)))
                body))
        (error body)))))

(defun papis--http-get-json (url &optional timeout)
  "GET URL and parse the response as JSON.

TIMEOUT defaults to `papis-request-timeout'.  Signal
`papis-http-error' for transport, HTTP and JSON errors."
  (let* ((timeout (or timeout papis-request-timeout))
         ;; The Papis server is local.  Do not send its requests through an
         ;; HTTP proxy from the environment: a proxy would make localhost
         ;; requests hang until they time out.
         (url-proxy-services nil)
         (url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "application/json")))
         (buffer (condition-case err
                     (url-retrieve-synchronously url t t timeout)
                   (error
                    (signal 'papis-http-error
                            (list (format "cannot reach %s: %s"
                                          url (error-message-string err))))))))
    (unless buffer
      (signal 'papis-http-error
              (list (format "request to %s timed out" url))))
    (when papis-debug
      (message "Papis: GET %s" url))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (let ((status (bound-and-true-p url-http-response-status))
                (body nil))
            (if (search-forward-regexp "\r?\n\r?\n" nil t)
                (setq body (buffer-substring-no-properties (point) (point-max)))
              (setq body ""))
            (setq body (decode-coding-string body 'utf-8))
            ;; `papis serve' sends the headers of a successful response and
            ;; only then hits an error while writing the JSON body.  The
            ;; error is another HTTP response appended to the body.  Pick
            ;; the inner status and body so the caller sees the real error.
            (when (string-match "\\`HTTP/[0-9.]+[ \t]+\\([0-9]+\\)" body)
              (let ((inner-status (string-to-number (match-string 1 body))))
                (when (string-match "\r?\n\r?\n" body)
                  (setq status inner-status
                        body (substring body (match-end 0))))))
            (cond
             ((not (and (integerp status) (<= 200 status) (< status 300)))
              (signal 'papis-http-error
                      (list (format "%s (HTTP %s)"
                                    (papis--error-message body)
                                    (or status "unknown")))))
             ((string-empty-p (string-trim body)) nil)
             (t
              (condition-case err
                  (json-parse-string body
                                     :object-type 'alist
                                     :array-type 'list
                                     :null-object nil
                                     :false-object nil)
                (error
                 (signal 'papis-http-error
                         (list (format "invalid JSON from %s: %s"
                                       url (error-message-string err))))))))))
      (kill-buffer buffer))))

(defun papis--request (path)
  "Request PATH from the Papis server and return parsed JSON."
  (papis-ensure-server)
  (papis--http-get-json (papis-server-url path)))

;;; Libraries

(defun papis-libraries ()
  "Return the names of all Papis libraries."
  (let ((data (papis--request "/api/library")))
    (unless (and (listp data) (cl-every #'stringp data))
      (signal 'papis-error (list "Unexpected response from /api/library")))
    data))

(defun papis-library (library)
  "Return a plist with :name and :path for LIBRARY."
  (let ((data (papis--request
               (format "/api/library/%s" (url-hexify-string library)))))
    (unless (listp data)
      (signal 'papis-error (list "Unexpected response from /api/library")))
    (list :name (alist-get 'name data)
          :path (alist-get 'path data))))

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

(defun papis--value-to-string (value)
  "Convert a JSON metadata VALUE to a human-readable string."
  (cond
   ((null value) "")
   ((stringp value) value)
   ((listp value)
    (string-join (mapcar #'papis--value-to-string value) ", "))
   ((numberp value) (number-to-string value))
   (t (format "%s" value))))

(defun papis-document-get (document key &optional default)
  "Return KEY from DOCUMENT's metadata, or DEFAULT.

KEY may be a string, a symbol or a keyword.  JSON object keys are
parsed by Emacs as symbols, so strings are interned for lookup."
  (let* ((key (cond
               ((keywordp key) (intern (substring (symbol-name key) 1)))
               ((stringp key) (intern key))
               (t key)))
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

(defun papis--document-path (library query)
  "Return the API path for QUERY in LIBRARY.

An empty QUERY requests every document."
  (let ((base (format "/api/library/%s/document"
                      (url-hexify-string library))))
    (if (or (null query) (string-empty-p (string-trim query)))
        base
      (concat base "/" (url-hexify-string (string-trim query))))))

(defun papis--parse-documents (data library)
  "Turn parsed JSON DATA into a list of `papis-document' for LIBRARY."
  (unless (cl-every #'listp data)
    (signal 'papis-error
            (list "Unexpected document response from the Papis server")))
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

(defun papis--query-ids (library query)
  "Return the ids matching QUERY in LIBRARY, using `papis list'."
  (let* ((query (string-trim (or query "")))
         (arguments (append (list "-l" library "list" "-a" "--id")
                            (unless (string-empty-p query)
                              (list query))))
         (output (papis--call-cli arguments)))
    (delq nil
          (mapcar (lambda (line)
                    (let ((line (string-trim line)))
                      (and (string-match-p "\\`[0-9a-fA-F-]+\\'" line)
                           line)))
                  (split-string output "\n")))))

(defun papis--serialization-error-p (error)
  "Return non-nil when ERROR is a Papis JSON serialization error."
  (string-match-p "serializ" (papis-error-message error)))

(defun papis--documents-via-python (library query)
  "Fetch documents matching QUERY in LIBRARY with the Python helper.

This avoids the JSON serialization bug in the HTTP API by running
`papis-dump-script' through `papis exec'."
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

(defun papis--documents-via-cli-ids (library query)
  "Fetch documents matching QUERY in LIBRARY one id at a time.

Used as a workaround when the server cannot serialize the full result
and the Python helper is unavailable."
  (let ((ids (papis--query-ids library query)))
    (when ids
      (message "Papis: fetching %d document%s by id to work around a server serialization error..."
               (length ids) (if (= (length ids) 1) "" "s")))
    (delq nil
          (mapcar (lambda (id)
                    (car (papis--parse-documents
                          (papis--request
                           (papis--document-path
                            library (format "papis_id:%s" id)))
                          library)))
                  ids))))

(defun papis--fallback-documents (library query)
  "Return documents for LIBRARY and QUERY through a fallback path."
  (if (file-exists-p papis-dump-script)
      (condition-case nil
          (papis--documents-via-python library query)
        (error (papis--documents-via-cli-ids library query)))
    (papis--documents-via-cli-ids library query)))

(defun papis-documents (&optional library query)
  "Return the documents of LIBRARY matching QUERY.

QUERY is a normal Papis query, such as \"author:knuth year:1984\".
When QUERY is nil or empty, all documents are returned.  LIBRARY
defaults to `papis-default-library' or the first Papis library."
  (papis-ensure-server)
  (let* ((library (or library (papis--default-library)))
         (query (or query "")))
    (unless library
      (signal 'papis-error
              (list "No Papis library found; configure one with `papis init' or Papis' config file")))
    (condition-case err
        (papis--parse-documents
         (papis--request (papis--document-path library query))
         library)
      (papis-error
       (if (and papis-fallback-to-cli
                (papis--serialization-error-p err))
           (papis--fallback-documents library query)
         (signal (car err) (cdr err)))))))

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

(defun papis--document-set-tags (document tags)
  "Store TAGS in DOCUMENT's in-memory metadata and return them.

This keeps the document in the frontend in sync with the tag
command; the Papis server itself may need a restart before it
returns the new tags."
  (let ((cell (assoc 'tags (papis-document-data document))))
    (if cell
        (setcdr cell tags)
      (setf (papis-document-data document)
            (cons (cons 'tags tags) (papis-document-data document))))
    tags))

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

(defun papis-document-restart-server ()
  "Restart the Papis server when this Emacs started it.

`papis serve' keeps its library in memory, so metadata written by
the `papis' command line is not visible to the API until the
server restarts.  A server started by another program is left
alone.  Return non-nil when the server was restarted."
  (when (process-live-p papis--server-process)
    (papis-restart-server)
    t))

;;; Book metadata

(defun papis-document-book-files (document)
  "Return the EPUB and PDF files attached to DOCUMENT."
  (seq-filter (lambda (file)
                (string-match-p "\\.\\(?:epub\\|pdf\\)\\'" (downcase file)))
              (papis-document-files document)))

(defun papis-document-book-metadata-command (file)
  "Return the command list that runs the metadata script on FILE.

The first element is the program and the rest are its arguments.
The script is executed directly when it is executable (its shebang
runs `uv'), otherwise it is run through `uv run --script'."
  (let ((script (expand-file-name papis-book-metadata-script)))
    (unless (file-exists-p script)
      (signal 'papis-error
              (list (format "Book metadata script not found: %s" script))))
    (cond
     ((file-executable-p script)
      (list script file))
     ((executable-find "uv")
      (list (executable-find "uv") "run" "--script" script file))
     (t
      (signal 'papis-error
              (list (format "Cannot run %s; make it executable or install `uv'"
                            script)))))))

(defun papis--run-book-metadata-script (file)
  "Run the book metadata script on FILE and return its output.

Signal `papis-error' when the script exits unsuccessfully."
  (let* ((command (papis-document-book-metadata-command file))
         (stderr-file (make-temp-file "papis-book-metadata-stderr-"))
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
to the server until the cache is updated.  Rebuild only DOCUMENT's
entry when its folder is known, otherwise update all newer entries
in the library."
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
afterwards so the server returns the new metadata.  Return the
script output."
  (let ((file (or file (car (papis-document-book-files document)))))
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

(defun papis-document-sync-metadata (document)
  "Sync DOCUMENT's metadata after `info.yaml' was edited.

When DOCUMENT has an EPUB or PDF file, run the book metadata script
so the file is updated and renamed.  Otherwise only refresh the
Papis cache."
  (if (papis-document-book-files document)
      (papis-document-update-book-metadata document)
    (papis-document-refresh-cache document)))

;;; Export

(defun papis-document-format (library query format)
  "Export documents matching QUERY in LIBRARY using Papis exporter FORMAT.

FORMAT is a Papis exporter name, such as \"bibtex\" or \"yaml\".
Return the exported string."
  (let* ((query (string-trim (or query "")))
         (path (if (string-empty-p query)
                   ;; The API route needs a non-empty query segment, so use
                   ;; an encoded space, which Papis treats as an empty query.
                   (format "/api/library/%s/document/%%20"
                           (url-hexify-string library))
                 (papis--document-path library query))))
    (papis--request
     (format "%s/format/%s" path (url-hexify-string format)))))

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
  "Export DOCUMENT using Papis exporter FORMAT.

Try the server API first.  Papis 0.16 has a broken format endpoint
\(plugin loading returns an empty string), so fall back to
`papis export' when the API returns nothing."
  (let* ((library (or (papis-document-library document)
                      (papis--default-library)))
         (id (papis-document-id document))
         (query (and id (format "papis_id:%s" id)))
         (result (when query
                   (condition-case nil
                       (papis-document-format library query format)
                     (error nil)))))
    (if (and (stringp result)
             (not (string-empty-p (string-trim result))))
        result
      (papis--export-via-cli document format))))

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
