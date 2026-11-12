;;; papis-etaf.el --- ETAF gallery frontend for Papis  -*- lexical-binding: t; -*-

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

;; A cover gallery for Papis, built with ETAF.  It reuses the plain
;; data layer in `papis.el' and renders books as a responsive grid of
;; covers, titles, authors and tag pills.  Covers are taken from EPUB
;; metadata, the first page of PDFs, or generated from the document
;; metadata, and are cached on disk.
;;
;; Start the gallery with `M-x papis-etaf-open'.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'dom)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'papis)
(require 'papis-openlibrary)
(require 'etaf)

(defgroup papis-etaf nil
  "ETAF gallery frontend for Papis."
  :group 'papis
  :prefix "papis-etaf-")

(defcustom papis-etaf-buffer-name "*papis-etaf*"
  "Name of the buffer used by the Papis cover gallery."
  :type 'string
  :group 'papis-etaf)

(defcustom papis-etaf-cover-directory
  (expand-file-name "papis/etaf-covers"
                    (or (getenv "XDG_CACHE_HOME")
                        (expand-file-name "~/.cache")))
  "Directory where generated and extracted book covers are cached."
  :type 'directory
  :group 'papis-etaf)

(defcustom papis-etaf-card-min-width 150
  "Minimum width of one book card in pixels.

The gallery fits as many columns of this width as the window allows.
A smaller value makes each card (and, with the 1.5 cover ratio, its
height) smaller."
  :type 'integer
  :group 'papis-etaf)

(defcustom papis-etaf-max-columns 8
  "Maximum number of book cards in one gallery row.

More columns make each card smaller while keeping the 1.5 cover
aspect."
  :type 'integer
  :group 'papis-etaf)

(defcustom papis-etaf-gap 24
  "Gap between gallery cards in pixels."
  :type 'integer
  :group 'papis-etaf)

(defcustom papis-etaf-page-margin 6
  "Horizontal page margin in characters."
  :type 'integer
  :group 'papis-etaf)

(defcustom papis-etaf-cover-aspect 1.5
  "Height of a book cover as a multiple of its width."
  :type 'number
  :group 'papis-etaf)

(defcustom papis-etaf-unzip-program "unzip"
  "Program used to read EPUB archives."
  :type 'string
  :group 'papis-etaf)

(defcustom papis-etaf-visible-rows 2
  "Number of book rows visible on the gallery page.

The list scrolls vertically for any remaining rows.  The page height
follows this value instead of a fixed fraction of the window."
  :type 'integer
  :group 'papis-etaf)

(defconst papis-etaf--cover-version 4
  "Version of the cover cache format.")

(defvar papis-etaf--cover-cache (make-hash-table :test #'equal)
  "Cache of cover signatures to cover file names.")

;;; Colors

(defun papis-etaf--face-color (face attribute fallback)
  "Return FACE's ATTRIBUTE as a color string, or FALLBACK."
  (let ((value (ignore-errors (face-attribute face attribute nil t))))
    (if (and (stringp value)
             (not (string-empty-p value))
             (not (string-prefix-p "unspecified" value)))
        value
      fallback)))

(defun papis-etaf--dark-background-p (color)
  "Return non-nil when COLOR is a dark color."
  (ignore-errors (color-dark-p (color-name-to-rgb color))))

(defun papis-etaf--palette ()
  "Return the color palette of the current theme."
  (let* ((background (papis-etaf--face-color 'default :background "#ffffff"))
         (foreground (papis-etaf--face-color 'default :foreground "#24292f"))
         (dark (papis-etaf--dark-background-p background))
         (accent (papis-etaf--face-color 'link :foreground
                                         (if dark "#79b8ff" "#2f5fa4")))
         (border (papis-etaf--face-color 'border :background
                                         (if dark "#3d444d" "#d0d7de")))
         (muted (papis-etaf--face-color 'shadow :foreground
                                        (if dark "#8b949e" "#57606a"))))
    (list :bg background
          :fg foreground
          :muted muted
          :border border
          :link accent
          :accent accent
          :accent-soft (color-lighten-name accent (if dark 78 82))
          :subtle (if dark
                      (color-lighten-name background 6)
                    (color-darken-name background 3))
          :input-bg (if dark
                        (color-lighten-name background 9)
                      (color-darken-name background 2))
          :danger-bg (papis-etaf--face-color 'error :background
                                             (if dark "#3d1d1f" "#ffebe9"))
          :danger-fg (papis-etaf--face-color 'error :foreground
                                             (if dark "#ffb3ad" "#a40e26")))))

;;; Covers

(defun papis-etaf--image-extension-p (file)
  "Return non-nil when FILE has an image extension."
  (string-match-p "\\.\\(?:jpe?g\\|png\\|gif\\|webp\\|bmp\\|svg\\)\\'"
                  (downcase (or file ""))))

(defun papis-etaf--valid-image-file-p (file)
  "Return non-nil when FILE is a readable image Emacs can display."
  (and (stringp file)
       (file-regular-p file)
       (file-readable-p file)
       (> (or (file-attribute-size (file-attributes file)) 0) 0)
       (let ((type (or (image-type-from-file-header file)
                       (image-supported-file-p file))))
         (and type (image-type-available-p type)))))

(defun papis-etaf--document-files (document)
  "Return the attached files of DOCUMENT with absolute names."
  (let ((directory (or (papis-document-folder document)
                       (papis-document-directory document)
                       default-directory)))
    (delq nil
          (mapcar (lambda (file)
                    (if (file-name-absolute-p file)
                        file
                      (expand-file-name file directory)))
                  (papis-document-files document)))))

(defun papis-etaf--cover-source (document)
  "Return the file that should provide the cover of DOCUMENT."
  (let* ((files (seq-filter #'file-exists-p (papis-etaf--document-files document)))
         (cover-image
          (seq-find (lambda (file)
                      (and (papis-etaf--image-extension-p file)
                           (string-match-p "cover"
                                           (downcase (file-name-nondirectory file)))))
                    files))
         (epub (seq-find (lambda (file)
                           (string-match-p "\\.epub\\'" (downcase file)))
                         files))
         (pdf (seq-find (lambda (file)
                          (string-match-p "\\.pdf\\'" (downcase file)))
                        files))
         (image (seq-find #'papis-etaf--image-extension-p files)))
    (or cover-image epub pdf image)))

(defun papis-etaf--document-signature (document)
  "Return the cache signature of DOCUMENT's cover."
  (let* ((source (papis-etaf--cover-source document))
         (attributes (and source (file-attributes source)))
         (mtime (and attributes
                     (float-time (file-attribute-modification-time attributes)))))
    (list papis-etaf--cover-version
          source
          (and attributes (file-attribute-size attributes))
          mtime
          (papis-document-title document)
          (papis-document-author document)
          (papis-document-file-type document))))

(defun papis-etaf--unzip-entry (archive entry)
  "Return the contents of ENTRY in ARCHIVE as a string, or nil."
  (when-let* ((program (executable-find papis-etaf-unzip-program)))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8))
        (let ((status (call-process program nil t nil "-p" archive entry)))
          (when (and (integerp status)
                     (zerop status)
                     (> (buffer-size) 0))
            (buffer-string)))))))

(defun papis-etaf--unzip-entry-to-file (archive entry destination)
  "Extract ENTRY from ARCHIVE to DESTINATION and return non-nil on success."
  (when-let* ((program (executable-find papis-etaf-unzip-program)))
    (let ((coding-system-for-read 'binary)
          (coding-system-for-write 'binary))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (let ((status (call-process program nil t nil "-p" archive entry)))
          (when (and (integerp status)
                     (zerop status)
                     (> (buffer-size) 0))
            (write-region (point-min) (point-max) destination nil 'silent)
            t))))))

(defun papis-etaf--epub-entries (file)
  "Return the archive entries of EPUB FILE."
  (when-let* ((program (executable-find papis-etaf-unzip-program)))
    (let ((coding-system-for-read 'utf-8))
      (ignore-errors (process-lines program "-Z1" file)))))

(defun papis-etaf--epub-resolve (base href)
  "Resolve EPUB HREF against BASE inside the archive."
  (let* ((href (url-unhex-string (replace-regexp-in-string "\\\\" "/" href)))
         (path (expand-file-name href (concat "/" (or base "")))))
    (string-remove-prefix "/" path)))

(defun papis-etaf--epub-xml (file entry)
  "Parse ENTRY of EPUB FILE and return its XML tree, or nil."
  (when-let* ((text (papis-etaf--unzip-entry file entry)))
    (condition-case nil
        (with-temp-buffer
          (insert text)
          (libxml-parse-xml-region (point-min) (point-max)))
      (error nil))))

(defun papis-etaf--epub-xhtml-image (file entry)
  "Return the first image entry referenced by XHTML ENTRY in FILE."
  (when-let* ((text (papis-etaf--unzip-entry file entry))
              (base (file-name-directory entry)))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (when (re-search-forward
             (concat "<\\(?:img\\|image\\)\\b[^>]*?"
                     "\\(?:src\\|xlink:href\\|href\\)=[\"']\\([^\"']+\\)[\"']")
             nil t)
        (papis-etaf--epub-resolve base (match-string 1))))))

(defun papis-etaf--epub-opf-cover (file)
  "Return the cover image entry of EPUB FILE, or nil."
  (when-let* ((entries (papis-etaf--epub-entries file))
              (opf (seq-find (lambda (entry)
                               (string-match-p "\\.opf\\'" (downcase entry)))
                             entries)))
    (let* ((root (papis-etaf--epub-xml file opf))
           (base (file-name-directory opf))
           (items (and root (dom-by-tag root 'item)))
           (metas (and root (dom-by-tag root 'meta)))
           (guide (and root (dom-by-tag root 'reference)))
           (cover-id
            (seq-some (lambda (meta)
                        (and (equal (dom-attr meta 'name) "cover")
                             (dom-attr meta 'content)))
                      metas))
           (item-by-id
            (lambda (id)
              (seq-find (lambda (item)
                          (equal (dom-attr item 'id) id))
                        items)))
           (cover-attr
            (seq-some (lambda (item)
                        (when (string-match-p
                               "cover-image"
                               (or (dom-attr item 'properties) ""))
                          (dom-attr item 'href)))
                      items))
           (meta-href
            (when cover-id
              (when-let* ((item (funcall item-by-id cover-id)))
                (dom-attr item 'href))))
           (named-href
            (seq-some
             (lambda (item)
               (let ((id (or (dom-attr item 'id) ""))
                     (href (or (dom-attr item 'href) ""))
                     (media (or (dom-attr item 'media-type) "")))
                 (when (and (string-match-p "^image/" media)
                            (string-match-p "cover" (concat id " " href)))
                   href)))
             items))
           (guide-href
            (seq-some (lambda (reference)
                        (when (equal (dom-attr reference 'type) "cover")
                          (dom-attr reference 'href)))
                      guide))
           (href (or cover-attr meta-href named-href guide-href)))
      (when href
        (let ((entry (papis-etaf--epub-resolve base href)))
          (if (string-match-p "\\.x?html?\\'" (downcase entry))
              (papis-etaf--epub-xhtml-image file entry)
            entry))))))

(defun papis-etaf--epub-cover-entry (file)
  "Return the archive entry that holds the cover of EPUB FILE."
  (or (papis-etaf--epub-opf-cover file)
      (seq-find (lambda (entry)
                  (and (papis-etaf--image-extension-p entry)
                       (string-match-p "cover"
                                       (downcase (file-name-nondirectory entry)))))
                (papis-etaf--epub-entries file))))

(defun papis-etaf--epub-cover (file key)
  "Extract the cover of EPUB FILE using cache KEY."
  (when-let* ((entry (papis-etaf--epub-cover-entry file)))
    (let* ((extension (downcase (or (file-name-extension entry) "jpg")))
           (destination (expand-file-name (concat key "." extension)
                                          papis-etaf-cover-directory)))
      (if (papis-etaf--valid-image-file-p destination)
          destination
        (when (papis-etaf--unzip-entry-to-file file entry destination)
          (if (papis-etaf--valid-image-file-p destination)
              destination
            (ignore-errors (delete-file destination))
            nil))))))

(defun papis-etaf--pdf-cover (file key)
  "Render the first page of PDF FILE as a cover using cache KEY."
  (let ((destination (expand-file-name (concat key ".png")
                                       papis-etaf-cover-directory))
        (prefix (expand-file-name key papis-etaf-cover-directory)))
    (if (papis-etaf--valid-image-file-p destination)
        destination
      (ignore-errors (delete-file destination))
      (when (executable-find "pdftoppm")
        (ignore-errors
          (call-process "pdftoppm" nil nil nil
                        "-f" "1" "-l" "1"
                        "-scale-to" "900"
                        "-singlefile" "-png" file prefix)))
      (unless (papis-etaf--valid-image-file-p destination)
        (when (executable-find "mutool")
          (ignore-errors
            (call-process "mutool" nil nil nil
                          "draw" "-o" destination "-F" "png" "-r" "120"
                          file "1"))))
      (when (papis-etaf--valid-image-file-p destination)
        destination))))

(defun papis-etaf--xml-escape (string)
  "Escape STRING for use in XML text."
  (replace-regexp-in-string
   "[&<>\"']"
   (lambda (match)
     (cdr (assoc match '(("&" . "&amp;")
                         ("<" . "&lt;")
                         (">" . "&gt;")
                         ("\"" . "&quot;")
                         ("'" . "&apos;")))))
   (or string "")
   t t))

(defun papis-etaf--take-width (string width)
  "Return the prefix of STRING that fits in WIDTH display columns."
  (truncate-string-to-width string width))

(defun papis-etaf--wrap-words (text width)
  "Wrap TEXT into lines of at most WIDTH display columns."
  (let ((words (split-string (or text "") "[ \t\n]+" t))
        (lines nil)
        (line nil))
    (dolist (word words)
      (while (> (string-width word) width)
        (let* ((part (or (papis-etaf--take-width word width) ""))
               (length (length part)))
          (when line
            (push line lines)
            (setq line nil))
          (when (not (string-empty-p part))
            (push part lines))
          (setq word (if (> (length word) length)
                         (substring word length)
                       ""))))
      (unless (string-empty-p word)
        (cond
         ((null line)
          (setq line word))
         ((<= (+ (string-width line) 1 (string-width word)) width)
          (setq line (concat line " " word)))
         (t
          (push line lines)
          (setq line word)))))
    (when line
      (push line lines))
    (nreverse lines)))

(defconst papis-etaf--cover-palettes
  '(("#1e3a5f" "#f8fafc" "#7dd3fc")
    ("#3b2f2f" "#f5e6d3" "#d4a373")
    ("#1f3d2e" "#ecfdf5" "#6ee7b7")
    ("#2d1b3d" "#f5e8ff" "#c4b5fd")
    ("#4a1f24" "#fff1f2" "#fda4af")
    ("#1f2937" "#f9fafb" "#93c5fd")
    ("#3d2c1e" "#fffbeb" "#fcd34d"))
  "Background, foreground and accent colors of generated covers.")

(defun papis-etaf--cover-palette (title)
  "Return a deterministic cover palette for TITLE."
  (let* ((hash (secure-hash 'sha1 (or title "")))
         (index (mod (string-to-number (substring hash 0 8) 16)
                     (length papis-etaf--cover-palettes))))
    (nth index papis-etaf--cover-palettes)))

(defun papis-etaf--placeholder-svg (document)
  "Return an SVG cover for DOCUMENT built from its metadata."
  (let* ((title (papis-document-title document))
         (author (papis-document-author document))
         (type (or (papis-document-file-type document) "BOOK"))
         (year (papis-document-year-string document))
         (palette (papis-etaf--cover-palette title))
         (background (nth 0 palette))
         (foreground (nth 1 palette))
         (accent (nth 2 palette))
         (lines (seq-take (papis-etaf--wrap-words title 16) 4))
         (line-count (length lines))
         (start-y (- 440 (* (1- line-count) 36)))
         (author (if (string-empty-p author) year author))
         (escaped-author (papis-etaf--xml-escape author))
         (escaped-type (papis-etaf--xml-escape type))
         (title-markup
          (let ((line-y start-y))
            (mapconcat
             (lambda (line)
               (prog1
                   (format "<text x=\"300\" y=\"%d\" fill=\"%s\" font-size=\"54\" font-weight=\"700\" text-anchor=\"middle\" font-family=\"Helvetica, Arial, sans-serif\">%s</text>"
                           line-y foreground (papis-etaf--xml-escape line))
                 (setq line-y (+ line-y 72))))
             lines
             "\n    "))))
    (format "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"600\" height=\"900\" viewBox=\"0 0 600 900\">
  <defs>
    <linearGradient id=\"bg\" x1=\"0\" y1=\"0\" x2=\"1\" y2=\"1\">
      <stop offset=\"0%%\" stop-color=\"%s\"/>
      <stop offset=\"100%%\" stop-color=\"#111827\"/>
    </linearGradient>
  </defs>
  <rect width=\"600\" height=\"900\" fill=\"url(#bg)\"/>
  <circle cx=\"500\" cy=\"160\" r=\"220\" fill=\"%s\" opacity=\"0.18\"/>
  <circle cx=\"80\" cy=\"760\" r=\"180\" fill=\"%s\" opacity=\"0.16\"/>
  <path d=\"M0 640 C 150 560, 430 700, 600 590 L600 900 L0 900 Z\" fill=\"%s\" opacity=\"0.18\"/>
  <rect x=\"38\" y=\"38\" width=\"524\" height=\"824\" fill=\"none\" stroke=\"%s\" stroke-width=\"2\" opacity=\"0.5\"/>
  <text x=\"60\" y=\"86\" fill=\"%s\" font-size=\"22\" font-weight=\"700\" letter-spacing=\"4\" font-family=\"Helvetica, Arial, sans-serif\">%s</text>
  <line x1=\"60\" y1=\"108\" x2=\"180\" y2=\"108\" stroke=\"%s\" stroke-width=\"3\"/>
  %s
  <line x1=\"60\" y1=\"760\" x2=\"540\" y2=\"760\" stroke=\"%s\" stroke-width=\"1\" opacity=\"0.55\"/>
  <text x=\"60\" y=\"818\" fill=\"%s\" font-size=\"30\" font-family=\"Helvetica, Arial, sans-serif\">%s</text>
</svg>"
            background accent accent accent accent foreground escaped-type accent
            title-markup accent foreground escaped-author)))

(defun papis-etaf--write-placeholder-cover (document key)
  "Write a generated cover for DOCUMENT using cache KEY."
  (let ((destination (expand-file-name (concat key ".svg")
                                       papis-etaf-cover-directory)))
    (unless (papis-etaf--valid-image-file-p destination)
      (ignore-errors (delete-file destination))
      (let ((coding-system-for-write 'utf-8-unix))
        (with-temp-file destination
          (insert (papis-etaf--placeholder-svg document)))))
    (when (papis-etaf--valid-image-file-p destination)
      destination)))

(defun papis-etaf--cached-cover (key)
  "Return the cached cover file for KEY, if it still exists."
  (let ((cached (gethash key papis-etaf--cover-cache)))
    (cond
     ((and cached (papis-etaf--valid-image-file-p cached)) cached)
     ((file-directory-p papis-etaf-cover-directory)
      (when-let* ((file (seq-find #'papis-etaf--valid-image-file-p
                                  (directory-files
                                   papis-etaf-cover-directory t
                                   (concat "\\`" (regexp-quote key) "\\.")))))
        (puthash key file papis-etaf--cover-cache)
        file)))))

(defun papis-etaf--build-cover (document key)
  "Build the cover file of DOCUMENT using cache KEY."
  (let* ((source (papis-etaf--cover-source document))
         (extension (downcase (or (file-name-extension source) ""))))
    (or (cond
         ((papis-etaf--image-extension-p source)
          (and (papis-etaf--valid-image-file-p source) source))
         ((equal extension "epub")
          (papis-etaf--epub-cover source key))
         ((equal extension "pdf")
          (papis-etaf--pdf-cover source key)))
        (papis-etaf--write-placeholder-cover document key))))

(defun papis-etaf--cover-file (document)
  "Return the cover file of DOCUMENT, generating it when necessary."
  (let* ((signature (papis-etaf--document-signature document))
         (key (secure-hash 'sha1 (prin1-to-string signature))))
    (or (papis-etaf--cached-cover key)
        (let ((file (progn
                      (make-directory papis-etaf-cover-directory t)
                      (papis-etaf--build-cover document key))))
          (when file
            (puthash key file papis-etaf--cover-cache)
            file)))))

(defun papis-etaf-clear-cover-cache ()
  "Delete cached covers and forget their in-memory mapping."
  (interactive)
  (clrhash papis-etaf--cover-cache)
  (when (file-directory-p papis-etaf-cover-directory)
    (dolist (file (directory-files papis-etaf-cover-directory t
                                   "\\`[0-9a-f]+\\."))
      (ignore-errors (delete-file file))))
  (message "Cleared the Papis ETAF cover cache"))

;;; Session

(cl-defstruct (papis-etaf--session (:constructor papis-etaf--session-create))
  "Mutable state shared by the ETAF Papis gallery."
  libraries
  library
  query
  documents
  selected
  error
  covers)

(defvar papis-etaf--focused-document (etaf-ref nil)
  "Book under the keyboard cursor in the gallery.
Kept outside the session so reloading this file does not change the
session structure of an already mounted gallery.")

(defun papis-etaf--same-document-p (first second)
  "Return non-nil when FIRST and SECOND denote the same Papis document."
  (or (eq first second)
      (let ((first-id (and first (papis-document-id first)))
            (second-id (and second (papis-document-id second))))
        (and first-id second-id (equal first-id second-id)))))

(defun papis-etaf--current-document (&optional session)
  "Return the selected document of SESSION."
  (let ((session (or session (papis-etaf--app-session))))
    (or (etaf-value (papis-etaf--session-selected session))
        (car (etaf-value (papis-etaf--session-documents session))))))

(defun papis-etaf--document-key (document)
  "Return a cache key for DOCUMENT."
  (or (papis-document-id document)
      (papis-document-title document)
      (prin1-to-string (papis-document-fields document))))

(defun papis-etaf--prepare-covers (session documents)
  "Prepare the covers of DOCUMENTS and store them in SESSION."
  (let ((covers (papis-etaf--session-covers session)))
    (clrhash covers)
    (dolist (document documents)
      (condition-case nil
          (when-let* ((file (papis-etaf--cover-file document)))
            (puthash (papis-etaf--document-key document) file covers))
        (error nil)))))

(defun papis-etaf--session-cover (session document)
  "Return the prepared cover of DOCUMENT in SESSION."
  (gethash (papis-etaf--document-key document)
           (papis-etaf--session-covers session)))

(defun papis-etaf--reload (session &optional library query)
  "Reload SESSION for LIBRARY and QUERY."
  (let* ((library (or library
                      (etaf-value (papis-etaf--session-library session))))
         (query (or query
                    (etaf-value (papis-etaf--session-query session))))
         (previous (etaf-value (papis-etaf--session-selected session))))
    (condition-case err
        (let* ((documents (papis-documents library query))
               (selected (or (seq-find
                              (lambda (document)
                                (papis-etaf--same-document-p document previous))
                              documents)
                             (car documents))))
          (papis-etaf--prepare-covers session documents)
          (etaf-batch
            (etaf-set-value (papis-etaf--session-library session) library)
            (etaf-set-value (papis-etaf--session-query session) query)
            (etaf-set-value (papis-etaf--session-documents session) documents)
            (etaf-set-value (papis-etaf--session-selected session) selected)
            (etaf-set-value papis-etaf--focused-document nil)
            (etaf-set-value (papis-etaf--session-error session) nil)))
      (error
       (etaf-batch
         (etaf-set-value (papis-etaf--session-library session) library)
         (etaf-set-value (papis-etaf--session-query session) query)
         (etaf-set-value (papis-etaf--session-documents session) nil)
         (etaf-set-value (papis-etaf--session-selected session) nil)
         (etaf-set-value papis-etaf--focused-document nil)
         (etaf-set-value (papis-etaf--session-error session)
                         (papis-error-message err)))))))

;;; Commands and actions

(defun papis-etaf--app-session ()
  "Return the session of the current Papis ETAF application."
  (let ((state (and (fboundp 'etaf-app-state) (etaf-app-state))))
    (or (and (papis-etaf--session-p state) state)
        (user-error "No Papis ETAF application in the current buffer"))))

(defun papis-etaf-refresh ()
  "Reload the current library and query."
  (interactive)
  (papis-etaf--reload (papis-etaf--app-session)))

(defun papis-etaf--prompt-search (session)
  "Prompt for a Papis query and reload SESSION."
  (papis-etaf--reload
   session nil
   (read-string "Papis query: "
                (etaf-value (papis-etaf--session-query session)))))

(defun papis-etaf-search (&optional query)
  "Prompt for a Papis QUERY and reload the gallery."
  (interactive)
  (let ((session (papis-etaf--app-session)))
    (if query
        (papis-etaf--reload session nil query)
      (papis-etaf--prompt-search session))))

(defun papis-etaf-select-library (&optional library)
  "Switch the gallery to LIBRARY."
  (interactive)
  (let* ((session (papis-etaf--app-session))
         (libraries (etaf-value (papis-etaf--session-libraries session)))
         (library (or library
                      (completing-read "Library: " libraries nil t))))
    (papis-etaf--reload session library "")))

(defun papis-etaf-next-library ()
  "Switch to the next Papis library."
  (interactive)
  (let* ((session (papis-etaf--app-session))
         (libraries (etaf-value (papis-etaf--session-libraries session)))
         (current (etaf-value (papis-etaf--session-library session)))
         (tail (cdr (member current libraries)))
         (next (or (car tail) (car libraries))))
    (when next
      (papis-etaf--reload session next ""))))

(defun papis-etaf--select-next (session step)
  "Move the selection of SESSION by STEP documents."
  (let* ((documents (etaf-value (papis-etaf--session-documents session)))
         (current (etaf-value (papis-etaf--session-selected session)))
         (position (or (seq-position documents current
                                     #'papis-etaf--same-document-p)
                       -1))
         (length (length documents)))
    (when (> length 0)
      (etaf-set-value
       (papis-etaf--session-selected session)
       (nth (mod (+ position step) length) documents)))))

(defun papis-etaf-next-document (&optional n)
  "Select the Nth next document."
  (interactive "p")
  (papis-etaf--select-next (papis-etaf--app-session) (or n 1)))

(defun papis-etaf-previous-document (&optional n)
  "Select the Nth previous document."
  (interactive "p")
  (papis-etaf--select-next (papis-etaf--app-session)
                           (- (or n 1))))

(defun papis-etaf--call (function &optional document)
  "Call FUNCTION with DOCUMENT or the selected document."
  (let ((document (or document (papis-etaf--current-document))))
    (unless document
      (user-error "No Papis document is selected"))
    (condition-case err
        (funcall function document)
      (error (message "Papis: %s" (papis-error-message err))))))

(defun papis-etaf-open-file ()
  "Open the file of the selected document."
  (interactive)
  (papis-etaf--call #'papis-document-open-file))

(defun papis-etaf-open-url ()
  "Open the URL of the selected document."
  (interactive)
  (papis-etaf--call #'papis-document-open-url))

(defun papis-etaf-open-directory ()
  "Open the folder of the selected document."
  (interactive)
  (papis-etaf--call #'papis-document-open-directory))

(defun papis-etaf-open-info ()
  "Open the `info.yaml' file of the selected document."
  (interactive)
  (papis-etaf--call #'papis-document-open-info))

(defun papis-etaf--copy-bibtex-entry (document)
  "Copy the BibTeX entry of DOCUMENT to the kill ring."
  (let ((entry (papis-document-export document "bibtex")))
    (when (string-empty-p (string-trim entry))
      (error "Papis returned an empty BibTeX entry"))
    (kill-new entry)
    (message "Copied BibTeX for %s" (papis-document-title document))))

(defun papis-etaf-copy-bibtex ()
  "Copy a BibTeX entry for the selected document."
  (interactive)
  (papis-etaf--call #'papis-etaf--copy-bibtex-entry))

(defun papis-etaf-copy-reference ()
  "Copy a human-readable reference for the selected document."
  (interactive)
  (papis-etaf--call
   (lambda (document)
     (kill-new (papis-document-citation document))
     (message "Copied reference for %s" (papis-document-title document)))))

;;; Open Library metadata

(defun papis-etaf--replace-document (session document)
  "Replace DOCUMENT in SESSION with a copy and return the copy.

A card only re-renders when its document object changes, so metadata
edited in place needs a fresh object."
  (let* ((updated (papis-document-copy document))
         (documents (etaf-value (papis-etaf--session-documents session)))
         (selected (etaf-value (papis-etaf--session-selected session)))
         (focused (etaf-value papis-etaf--focused-document))
         (replaced (mapcar (lambda (each)
                             (if (papis-etaf--same-document-p each document)
                                 updated
                               each))
                           documents)))
    (etaf-batch
      (etaf-set-value (papis-etaf--session-documents session) replaced)
      (when (papis-etaf--same-document-p selected document)
        (etaf-set-value (papis-etaf--session-selected session) updated))
      (when (papis-etaf--same-document-p focused document)
        (etaf-set-value papis-etaf--focused-document updated)))
    updated))

(defun papis-etaf-find-metadata ()
  "Look up the selected book on Open Library and apply its metadata."
  (interactive)
  (let* ((session (papis-etaf--app-session))
         (document (or (papis-etaf--current-document session)
                       (user-error "No book is selected")))
         (fields (papis-openlibrary-metadata-for-document document)))
    (when fields
      (papis-document-update document fields)
      (papis-etaf--reload session)
      (message "Updated \"%s\" from Open Library"
               (papis-document-title document)))))

(defun papis-etaf-update-book-metadata ()
  "Update the selected book's file metadata with the metadata script."
  (interactive)
  (let* ((session (papis-etaf--app-session))
         (document (or (papis-etaf--current-document session)
                       (user-error "No book is selected"))))
    (condition-case err
        (progn
          (papis-update-book-metadata document)
          (papis-etaf--reload session)
          (message "Updated the file metadata of \"%s\""
                   (papis-document-title document)))
      (error
       (message "Papis: %s" (papis-error-message err))))))

(defun papis-etaf-set-cover ()
  "Search Open Library for a cover of the selected book and attach it."
  (interactive)
  (let* ((session (papis-etaf--app-session))
         (document (or (papis-etaf--current-document session)
                       (user-error "No book is selected"))))
    (condition-case err
        (when-let* ((cover (papis-openlibrary-set-cover document)))
          (papis-etaf--replace-document session document)
          (papis-etaf--prepare-covers
           session (etaf-value (papis-etaf--session-documents session)))
          (message "Added %s to \"%s\""
                   (file-name-nondirectory cover)
                   (papis-document-title document)))
      (error
       (message "Papis: %s" (papis-error-message err))))))

;;; Rendering helpers

(defun papis-etaf--px (value)
  "Return VALUE as a pixel length."
  (list 'px value))

(defun papis-etaf--lh (value)
  "Return VALUE as a line-height length."
  (list 'lh value))

(defun papis-etaf--gap-layout (width)
  "Return (COLUMNS IMAGE-COLUMNS IMAGE-ROWS) for WIDTH pixels."
  (let* ((metrics (etaf-display-metrics))
         (char-width (max 8 (or (plist-get metrics :char-width) 8)))
         (line-height (max 18 (or (plist-get metrics :line-height) 18)))
         (gap papis-etaf-gap)
         (margin-width (* 2 papis-etaf-page-margin))
         (available (if (display-graphic-p)
                        (max 240 (- width (* margin-width char-width)))
                      (max 240 (* char-width
                                   (max 1 (- width margin-width))))))
         (columns (max 1
                       (min papis-etaf-max-columns
                            (floor (/ (+ available gap)
                                      (+ papis-etaf-card-min-width gap))))))
         (card-width (max 120
                          (floor (/ (- available (* gap (1- columns)))
                                    columns))))
         (image-columns (max 8 (floor (/ card-width char-width))))
         (image-rows (max 4
                          (round (/ (* image-columns char-width
                                      papis-etaf-cover-aspect)
                                    line-height)))))
    (list columns image-columns image-rows)))

(defun papis-etaf--truncate (string width)
  "Truncate STRING to WIDTH display columns with an ellipsis."
  (let ((string (or string "")))
    (if (<= (string-width string) width)
        string
      (truncate-string-to-width string width nil nil "…"))))

(defun papis-etaf--document-year-and-type (document)
  "Return a short year and file-type line for DOCUMENT."
  (string-join
   (delq nil
         (list (papis-document-year-string document)
               (papis-document-file-type document)))
   " · "))

(defun papis-etaf--document-file-name (document)
  "Return the display name of DOCUMENT's book file, or nil.

Prefer a file that actually exists in the document folder, so the
displayed name stays correct when the cached `files' value is stale."
  (when-let* ((file (or (car (papis-document--existing-book-files document))
                        (car (papis-document-book-files document))
                        (papis-document-file document))))
    (file-name-nondirectory file)))

(defun papis-etaf--tag-label (document)
  "Return the tag pill label of DOCUMENT."
  (let ((tags (papis-document-tags document)))
    (when tags
      (let ((label (if (> (length tags) 1)
                       (format "%s +%d" (car tags) (1- (length tags)))
                     (car tags))))
        (papis-etaf--truncate label 14)))))

(defun papis-etaf--action-button (label command disabled)
  "Return an action button view with LABEL, COMMAND and DISABLED state."
  (etaf-node
   'box
   (list :role 'button
         :tab-index (if disabled nil 0)
         :disabled disabled
         :padding '((lh 0) (ch 1))
         :border (list '(px 1) 'solid
                       (plist-get (papis-etaf--palette) :border))
         :background-color (plist-get (papis-etaf--palette) :input-bg)
         :color (plist-get (papis-etaf--palette) :fg)
         :hover-style (list :background-color
                            (plist-get (papis-etaf--palette) :accent-soft))
         :on-press command)
   (list label)))

(defun papis-etaf--keys ()
  "Return the root key bindings of the gallery."
  (list
   (list "/" #'papis-etaf-search "Search")
   (list "r" #'papis-etaf-refresh "Refresh")
   (list "g" #'papis-etaf-refresh "Refresh")
   (list "s" #'papis-etaf-next-library "Next library")
   (list "n" #'papis-etaf-next-document "Next document")
   (list "p" #'papis-etaf-previous-document "Previous document")
   (list "o" #'papis-etaf-open-file "Open file")
   (list "u" #'papis-etaf-open-url "Open URL")
   (list "d" #'papis-etaf-open-directory "Open folder")
   (list "i" #'papis-etaf-open-info "Open info file")
   (list "m" #'papis-etaf-find-metadata "Find metadata")
   (list "C" #'papis-etaf-set-cover "Find cover")
   (list "M" #'papis-etaf-update-book-metadata "Update file metadata")
   (list "b" #'papis-etaf-copy-bibtex "Copy BibTeX")
   (list "c" #'papis-etaf-copy-reference "Copy reference")
   (list "q" #'etaf-app-close "Quit")))

;;; Components

(defmacro papis-etaf--defcomponent (name arguments &rest clauses)
  "Define ETAF component NAME with ARGUMENTS and CLAUSES, reload-safe.
`etaf-define-component' signals when a component is registered twice,
which would make `load-file', `eval-buffer' or `eval-defun' fail on
the second evaluation of this file.  Registering through
`etaf-component-redefine-run' lets development reloads replace the
definition instead."
  (declare (indent 2))
  `(etaf-component-redefine-run
    (lambda ()
      (etaf-define-component ,name ,arguments ,@clauses))))

(papis-etaf--defcomponent papis-etaf-book
    (&key document session columns rows)
  :render
  (let* ((palette (papis-etaf--palette))
         (title (papis-document-title document))
         (author (papis-document-author document))
         (year-and-type (papis-etaf--document-year-and-type document))
         (tag (papis-etaf--tag-label document))
         (cover (papis-etaf--session-cover session document))
         (selected (etaf-value (papis-etaf--session-selected session)))
         (selected-p (papis-etaf--same-document-p document selected))
         (focused (etaf-value papis-etaf--focused-document))
         (focused-p (papis-etaf--same-document-p document focused))
         (highlight-p (or selected-p focused-p))
         (select (lambda () (etaf-set-value
                             (papis-etaf--session-selected session)
                             document)))
         (focus-in (lambda ()
                     (etaf-set-value papis-etaf--focused-document document)
                     (etaf-set-value (papis-etaf--session-selected session)
                                     document)))
         (focus-out (lambda ()
                      (when (papis-etaf--same-document-p
                             document
                             (etaf-value papis-etaf--focused-document))
                        (etaf-set-value papis-etaf--focused-document nil))))
         (title-width (max 8 (floor (* columns 0.72))))
         (author-label (if (string-empty-p author)
                           (if (string-empty-p year-and-type)
                               "Unknown author"
                             year-and-type)
                         author))
         (border (list '(px 1) 'solid
                       (if highlight-p
                           (plist-get palette :accent)
                         (plist-get palette :border))))
         (background (cond (focused-p (plist-get palette :accent))
                           (selected-p (plist-get palette :accent-soft))))
         (title-color (cond (focused-p (plist-get palette :bg))
                            (selected-p (plist-get palette :accent))
                            (t (plist-get palette :link))))
         (author-color (if focused-p
                           (plist-get palette :bg)
                         (plist-get palette :muted))))
    (etaf-view
     (box :class "papis-etaf-book"
          :width 'stretch
          :text-align 'center
          :background-color background
          :tab-index 0
          :role 'button
          :help-echo title
          :on-press select
          :on-focus-enter focus-in
          :on-focus-leave focus-out
       (column :class "papis-etaf-book-lines"
               :width 'stretch
               :item-gap '(lh 0)
         (box :class "papis-etaf-cover-frame"
              :position 'relative
              :width 'stretch
              :overflow 'hidden
              :border border
              :background-color (plist-get palette :subtle)
              :on-press select
           (image :if cover
                  :src cover :columns columns :rows rows
                  :on-press select)
           (box :else t
                :height (papis-etaf--lh rows)
                :background-color (nth 0 (papis-etaf--cover-palette title))
                :on-press select
             (text :font-size 42
                   :font-weight 'bold
                   :color (nth 1 (papis-etaf--cover-palette title))
                   (expr (substring title 0 (min 1 (length title))))))
           (box :if tag
                :position 'absolute
                :left (papis-etaf--px 10)
                :top (papis-etaf--lh (max 0 (- rows 2)))
                :padding '((lh 0) (ch 1))
                :background-color (plist-get palette :accent-soft)
                :color (plist-get palette :accent)
                :on-press select
             (text :color (plist-get palette :accent)
                   :font-weight 'bold
                   :font-size 12
                   (expr tag))))
         (text :color title-color
               :font-size 14
               :font-weight (if selected-p 'bold 'normal)
               :text-decoration-line (if highlight-p '(underline) nil)
               :on-press select
               (expr (concat (if selected-p "> " "  ")
                             (papis-etaf--truncate title (- title-width 2)))))
         (text :color author-color
               :font-size 12
               :on-press select
               (expr (papis-etaf--truncate author-label title-width))))))))

(papis-etaf--defcomponent papis-etaf-app (&key session)
  :render
  (let* ((palette (papis-etaf--palette))
         (libraries (etaf-value (papis-etaf--session-libraries session)))
         (library (etaf-value (papis-etaf--session-library session)))
         (query (etaf-value (papis-etaf--session-query session)))
         (documents (etaf-value (papis-etaf--session-documents session)))
         (selected (etaf-value (papis-etaf--session-selected session)))
         (error (etaf-value (papis-etaf--session-error session)))
         (viewport (etaf-viewport))
         (layout (papis-etaf--gap-layout (car viewport)))
         (columns (nth 0 layout))
         (image-columns (nth 1 layout))
         (image-rows (nth 2 layout))
         (visible-rows (max 1 papis-etaf-visible-rows))
         (books-rows (max 6 (+ (* visible-rows (+ image-rows 2))
                               (* (1- visible-rows) 1))))
         (entries (seq-map-indexed (lambda (document index)
                                      (cons index document))
                                    documents))
         (selected-title (and selected (papis-document-title selected)))
         (selected-meta (and selected
                             (string-join
                              (delq nil
                                    (list (papis-document-author selected)
                                          (papis-document-year-string selected)
                                          (papis-document-file-type selected)))
                              " · ")))
         (selected-file (and selected
                             (papis-etaf--document-file-name selected))))
    (etaf-view
     (column :class "papis-etaf-root"
             :width 'stretch
             :height 'stretch
             :overflow 'scroll
             :background-color (plist-get palette :bg)
             :color (plist-get palette :fg)
             :padding (list (papis-etaf--lh 1)
                            (list 'ch papis-etaf-page-margin))
             :item-gap '(lh 1)
             :keys (papis-etaf--keys)
       (flex :class "papis-etaf-header"
             :flex-flow '(row wrap)
             :align-items 'center
             :gap '((lh 0) (ch 2))
         (text :font-size 26 :font-weight 'bold "Papis")
         (text :color (plist-get palette :muted)
               :font-size 13
               (expr (format "%d book%s%s"
                             (length documents)
                             (if (= (length documents) 1) "" "s")
                             (if (string-empty-p query) "" " found"))))
         (text :color (plist-get palette :border) "│")
         (text :color (plist-get palette :muted) :font-size 13 "Library:")
         (box :for (name libraries) :key name
              :role 'button
              :tab-index 0
              :padding '((lh 0) (ch 1))
              :font-size 13
              :font-weight (if (equal name library) 'bold 'normal)
              :background-color (if (equal name library)
                                    (plist-get palette :accent-soft)
                                  nil)
              :color (if (equal name library)
                         (plist-get palette :accent)
                       (plist-get palette :muted))
              :on-press (lambda () (papis-etaf--reload session name ""))
           (expr name))
         (text :if (null libraries)
               :color (plist-get palette :muted)
               "none")
         (text :color (plist-get palette :border) "│")
         (box :role 'button
              :tab-index 0
              :padding '((lh 0) (ch 1))
              :border (list '(px 1) 'solid (plist-get palette :border))
              :background-color (plist-get palette :input-bg)
              :font-size 13
              :color (plist-get palette :fg)
              :hover-style (list :background-color
                                 (plist-get palette :accent-soft))
              :on-press (lambda () (papis-etaf--reload session))
           "Refresh")
         (text :color (plist-get palette :border) "│")
         (text :color (plist-get palette :muted)
               :font-size 12
               "Move or click a card to select · o open · d folder · i info · m OL metadata · C cover · M file metadata · b BibTeX · q quit"))
       (flex :class "papis-etaf-search-row"
             :flex-flow '(row wrap)
             :align-items 'center
             :gap '((lh 0) (ch 2))
         (box :role 'button
              :tab-index 0
              :width '(px 300)
              :padding '((lh 0) (ch 1))
              :border (list '(px 1) 'solid (plist-get palette :border))
              :background-color (plist-get palette :input-bg)
              :font-size 13
              :color (if (string-empty-p query)
                         (plist-get palette :muted)
                       (plist-get palette :fg))
              :hover-style (list :background-color
                                 (plist-get palette :accent-soft))
              :on-press (lambda () (papis-etaf--prompt-search session))
           (expr (if (string-empty-p query) "Search…" query)))
         (text :color (plist-get palette :muted)
               :font-size 12
               "Press / to search · s next library · r refresh"))
       (box :if error
            :padding '((lh 1) (ch 2))
            :border (list '(px 1) 'solid (plist-get palette :danger-fg))
            :background-color (plist-get palette :danger-bg)
         (text :color (plist-get palette :danger-fg)
               :font-weight 'bold
               (expr error)))
       (box :if (and (null documents) (null error))
            :padding '((lh 3) (ch 2))
         (text :color (plist-get palette :muted)
               :font-size 15
               "No documents match the current query."))
       (box :if documents
            :class "papis-etaf-books"
            :width 'stretch
            :height (papis-etaf--lh books-rows)
            :overflow 'scroll
            :tab-index 0
         (grid :class "papis-etaf-grid"
               :width 'stretch
               :grid-template-columns
               (make-list columns '(minmax (px 0) (fr 1)))
               :gap (list (papis-etaf--lh 1) (papis-etaf--px papis-etaf-gap))
           (papis-etaf-book :for (entry entries)
                            :key (car entry)
                            :document (cdr entry)
                            :session session
                            :columns image-columns
                            :rows image-rows)))
       (box :if selected
            :class "papis-etaf-selection"
            :width 'stretch
            :padding '((lh 1) (ch 2))
            :border (list '(px 1) 'solid (plist-get palette :border))
            :background-color (plist-get palette :subtle)
         (flex :flex-flow '(row wrap)
               :align-items 'center
               :justify-content 'space-between
               :gap '((lh 1) (ch 2))
           (column :item-gap '(lh 0)
             (text :font-size 16 :font-weight 'bold
                   (expr (papis-etaf--truncate selected-title 48)))
             (text :font-size 12 :color (plist-get palette :muted)
                   (expr selected-meta))
             (text :if selected-file
                   :font-size 12
                   :color (plist-get palette :muted)
                   (expr (papis-etaf--truncate selected-file 60))))
           (flex :flex-flow '(row wrap)
                 :align-items 'center
                 :gap '((lh 0) (ch 1))
             (expr (papis-etaf--action-button
                    "Open"
                    (lambda ()
                      (papis-etaf--call #'papis-document-open-file selected))
                    nil))
             (expr (papis-etaf--action-button
                    "Folder"
                    (lambda ()
                      (papis-etaf--call #'papis-document-open-directory
                                        selected))
                    nil))
             (expr (papis-etaf--action-button
                    "Info"
                    (lambda ()
                      (papis-etaf--call #'papis-document-open-info selected))
                    nil))
             (expr (papis-etaf--action-button
                    "Metadata"
                    (lambda () (papis-etaf-find-metadata))
                    nil))
             (expr (papis-etaf--action-button
                    "Cover"
                    (lambda () (papis-etaf-set-cover))
                    nil))
             (expr (papis-etaf--action-button
                    "File metadata"
                    (lambda () (papis-etaf-update-book-metadata))
                    nil))
             (expr (papis-etaf--action-button
                    "BibTeX"
                    (lambda ()
                      (papis-etaf--call #'papis-etaf--copy-bibtex-entry
                                        selected))
                    nil))
             (expr (papis-etaf--action-button
                    "URL"
                    (lambda ()
                      (papis-etaf--call #'papis-document-open-url selected))
                    (null (papis-document-url selected)))))))
))))

;;; Entry point

(defun papis-etaf--view (session)
  "Return the gallery view for SESSION."
  (etaf-view (papis-etaf-app :session session)))

(defun papis-etaf-open (&optional library query)
  "Open the Papis cover gallery for LIBRARY and QUERY."
  (interactive)
  (let* ((error nil)
         (libraries
          (condition-case err
              (papis-libraries)
            (error
             (setq error (papis-error-message err))
             nil)))
         (library (or library
                      (ignore-errors
                        (papis-default-library-name libraries))
                      (car libraries)))
         (query (or query ""))
         (documents
          (when (and library (null error))
            (condition-case err
                (papis-documents library query)
              (error
               (setq error (papis-error-message err))
               nil))))
         (session (papis-etaf--session-create
                   :libraries (etaf-ref libraries)
                   :library (etaf-ref library)
                   :query (etaf-ref query)
                   :documents (etaf-ref documents)
                   :selected (etaf-ref (car documents))
                   :error (etaf-ref error)
                   :covers (make-hash-table :test #'equal))))
    (etaf-set-value papis-etaf--focused-document nil)
    (when documents
      (papis-etaf--prepare-covers session documents))
    (etaf-app-open
     papis-etaf-buffer-name
     (lambda () (papis-etaf--view session))
     :state session
     :background (lambda () (plist-get (papis-etaf--palette) :bg))
     :foreground (lambda () (plist-get (papis-etaf--palette) :fg)))))

(defun papis-etaf (&optional library query)
  "Open the Papis cover gallery for LIBRARY and QUERY."
  (interactive)
  (papis-etaf-open library query))

(provide 'papis-etaf)
;;; papis-etaf.el ends here
