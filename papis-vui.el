;;; papis-vui.el --- VUI frontend for Papis  -*- lexical-binding: t; -*-

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

;; A browser for Papis documents built with vui.el.  This file only
;; consumes the functions from `papis.el' and turns the returned
;; documents into components: a search field, library and tag filters,
;; a sortable document table and a detail pane for the selected entry.
;;
;; Start it with `M-x papis-vui-open'.  The Papis server is started on
;; demand through `papis-ensure-server'.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'papis)
(require 'vui)
(require 'vui-components)

(defgroup papis-vui nil
  "VUI frontend for Papis."
  :group 'papis
  :prefix "papis-vui-")

(defcustom papis-vui-buffer-name "*papis*"
  "Name of the buffer used by the Papis frontend."
  :type 'string
  :group 'papis-vui)

(defcustom papis-vui-default-sort-field "title"
  "Field the document table is sorted by on startup."
  :type 'string
  :group 'papis-vui)

(defcustom papis-vui-default-sort-reverse nil
  "Whether the initial sort is descending."
  :type 'boolean
  :group 'papis-vui)

(defcustom papis-vui-sort-fields '("title" "author" "year" "date")
  "Fields offered as sort buttons."
  :type '(repeat string)
  :group 'papis-vui)

(defcustom papis-vui-tag-limit 20
  "Maximum number of tag buttons shown above the document table."
  :type 'integer
  :group 'papis-vui)

(defcustom papis-vui-text-lines 12
  "Lines of abstract or notes shown before the \"Show more\" button."
  :type 'integer
  :group 'papis-vui)

(defcustom papis-vui-width nil
  "Width used to lay out the buffer.

When nil, the width of the window showing the buffer is used."
  :type '(choice (const :tag "Window width" nil) integer)
  :group 'papis-vui)

;;; State

(cl-defstruct (papis-vui--state (:constructor papis-vui--state-create))
  libraries
  library
  library-path
  query
  input
  documents
  selected
  sort-field
  sort-reverse
  error
  expanded)

(defvar papis-vui--state nil
  "The current state of the Papis frontend.")

(defvar papis-vui--instance nil
  "The mounted VUI instance of the Papis frontend.")

(defconst papis-vui--hidden-metadata
  '("title" "abstract" "note" "notes" "files" "papis_id"
    "_papis_local_folder")
  "Metadata keys rendered separately or not at all in the detail pane.")

;;; Utilities

(defun papis-vui--width ()
  "Return the width to use for layout."
  (max 50 (or papis-vui-width
              (when-let* ((window (get-buffer-window papis-vui-buffer-name)))
                (window-width window))
              (window-width))))

(defun papis-vui--truncate (string width)
  "Truncate STRING to WIDTH display columns."
  (truncate-string-to-width (or string "") width nil nil "…"))

(defun papis-vui--rule ()
  "Return a horizontal rule vnode."
  (vui-text (make-string (papis-vui--width) ?─) :face 'shadow))

(defun papis-vui--wrap (text width)
  "Wrap TEXT into lines of at most WIDTH columns."
  (with-temp-buffer
    (insert (or text ""))
    (let ((fill-column (max 20 width)))
      (fill-region (point-min) (point-max)))
    (split-string (string-trim (buffer-string)) "\n" t)))

(defun papis-vui--call (function &rest arguments)
  "Call FUNCTION with ARGUMENTS, reporting errors in the echo area."
  (condition-case err
      (apply function arguments)
    (error
     (message "Papis: %s" (papis-error-message err)))))

(defun papis-vui--status (format-string &rest arguments)
  "Show a loading status from FORMAT-STRING and ARGUMENTS.
Force the message to be displayed before the next blocking call."
  (message "Papis: %s" (apply #'format format-string arguments))
  (redisplay t))

;;; Queries and loading

(defun papis-vui--fetch (state library query)
  "Load documents for LIBRARY and QUERY into STATE."
  (papis-vui--status "loading %s%s..."
                     (or library "documents")
                     (if (string-empty-p (or query ""))
                         ""
                       (format " (query: %s)" query)))
  (let ((previous (papis-vui--state-selected state))
        (resolved library))
    (condition-case err
        (progn
          (unless (papis-vui--state-libraries state)
            (setf (papis-vui--state-libraries state) (papis-libraries)))
          (setq resolved (or resolved
                             (papis-default-library-name
                              (papis-vui--state-libraries state))))
          (let ((documents (papis-documents resolved query))
                (previous-id (and previous (papis-document-id previous))))
            (setf (papis-vui--state-library state) resolved
                  (papis-vui--state-query state) query
                  (papis-vui--state-input state) query
                  (papis-vui--state-documents state) documents
                  (papis-vui--state-error state) nil
                  (papis-vui--state-selected state)
                  (or (and previous-id
                           (seq-find
                            (lambda (document)
                              (equal previous-id (papis-document-id document)))
                            documents))
                      (and previous
                           (seq-find
                            (lambda (document)
                              (equal (papis-document-data document)
                                     (papis-document-data previous)))
                            documents)))
                  (papis-vui--state-library-path state)
                  (ignore-errors (plist-get (papis-library resolved) :path)))
            (unless (papis-vui--state-selected state)
              (setf (papis-vui--state-selected state)
                    (car (papis-vui--sorted-documents state))))
            (papis-vui--status "%d document%s in %s"
                               (length documents)
                               (if (= (length documents) 1) "" "s")
                               resolved)))
      (error
       (setf (papis-vui--state-library state) resolved
             (papis-vui--state-query state) query
             (papis-vui--state-input state) query
             (papis-vui--state-documents state) nil
             (papis-vui--state-selected state) nil
             (papis-vui--state-error state) (papis-error-message err)
             (papis-vui--state-library-path state) nil)))
    state))

(defun papis-vui--initial-state (&optional library query)
  "Create the initial state, optionally for LIBRARY and QUERY."
  (let ((state (papis-vui--state-create
                :sort-field papis-vui-default-sort-field
                :sort-reverse papis-vui-default-sort-reverse
                :query (or query "")))
        (libraries nil))
    (condition-case err
        (setq libraries (papis-libraries))
      (error
       (setf (papis-vui--state-error state) (papis-error-message err))))
    (setf (papis-vui--state-libraries state) libraries)
    (cond
     ((null libraries)
      (unless (papis-vui--state-error state)
        (setf (papis-vui--state-error state)
              "No Papis libraries found.  Run `papis init' or add a library to the Papis config.")))
     (t
      (let ((library (or (and library (member library libraries) library)
                         (papis-default-library-name libraries))))
        (papis-vui--fetch state library (or query "")))))
    state))

(defun papis-vui--redraw ()
  "Re-render the mounted frontend from `papis-vui--state'."
  (when (and papis-vui--instance
             (buffer-live-p (vui-instance-buffer papis-vui--instance)))
    (vui-update papis-vui--instance
                (list :state papis-vui--state))))

(defun papis-vui--search (query)
  "Load documents matching QUERY in the current library."
  (when papis-vui--state
    (papis-vui--fetch papis-vui--state
                      (papis-vui--state-library papis-vui--state)
                      (string-trim (or query "")))
    (papis-vui--redraw)))

(defun papis-vui--select-library (library)
  "Switch to LIBRARY and list all of its documents."
  (when papis-vui--state
    (papis-vui--fetch papis-vui--state library "")
    (papis-vui--redraw)))

(defun papis-vui--sort-by (field)
  "Sort the document table by FIELD, toggling direction on repeat."
  (when papis-vui--state
    (if (equal field (papis-vui--state-sort-field papis-vui--state))
        (setf (papis-vui--state-sort-reverse papis-vui--state)
              (not (papis-vui--state-sort-reverse papis-vui--state)))
      (setf (papis-vui--state-sort-field papis-vui--state) field
            (papis-vui--state-sort-reverse papis-vui--state) nil))
    (papis-vui--redraw)))

(defun papis-vui--select-document (document)
  "Select DOCUMENT in the detail pane."
  (when papis-vui--state
    (setf (papis-vui--state-selected papis-vui--state) document
          (papis-vui--state-expanded papis-vui--state) nil)
    (papis-vui--redraw)))

;;; Sorting

(defun papis-vui--sort-value (document field)
  "Return the value of FIELD in DOCUMENT for sorting."
  (papis-document-get document field))

(defun papis-vui--sort-value-empty-p (document field)
  "Return non-nil when DOCUMENT has no value for FIELD."
  (let ((value (papis-vui--sort-value document field)))
    (or (null value) (equal value "") (equal value '()))))

(defun papis-vui--compare-documents (a b field)
  "Return non-nil when A sorts before B by FIELD."
  (let* ((va (papis-vui--sort-value a field))
         (vb (papis-vui--sort-value b field))
         (empty-a (papis-vui--sort-value-empty-p a field))
         (empty-b (papis-vui--sort-value-empty-p b field)))
    (cond
     ((and empty-a empty-b) nil)
     (empty-a nil)
     (empty-b t)
     ((and (numberp va) (numberp vb)) (< va vb))
     (t (string< (papis--value-to-string va)
                 (papis--value-to-string vb))))))

(defun papis-vui--sorted-documents (state)
  "Return the documents of STATE sorted by its current sort settings."
  (let ((documents (papis-vui--state-documents state))
        (field (papis-vui--state-sort-field state))
        (reverse (papis-vui--state-sort-reverse state)))
    (if (or (null field) (string-empty-p field))
        documents
      (sort (copy-sequence documents)
            (lambda (a b)
              (cond
               ((papis-vui--sort-value-empty-p a field) nil)
               ((papis-vui--sort-value-empty-p b field) t)
               (reverse (papis-vui--compare-documents b a field))
               (t (papis-vui--compare-documents a b field))))))))

(defun papis-vui--selected-document (state)
  "Return the selected document of STATE, falling back to the first one."
  (or (papis-vui--state-selected state)
      (car (papis-vui--sorted-documents state))))

(defun papis-vui--tag-counts (documents)
  "Return an alist of (TAG . COUNT) for DOCUMENTS."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (document documents)
      (dolist (tag (papis-document-tags document))
        (puthash tag (1+ (gethash tag table 0)) table)))
    (sort (mapcar (lambda (tag) (cons tag (gethash tag table)))
                  (hash-table-keys table))
          (lambda (a b)
            (if (= (cdr a) (cdr b))
                (string< (car a) (car b))
              (> (cdr a) (cdr b)))))))

(defun papis-vui--tag-query (tag)
  "Return a Papis query matching TAG."
  (if (string-match-p "[ \t\"]" tag)
      (format "tags:\"%s\"" (replace-regexp-in-string "\"" "\\\\\"" tag))
    (format "tags:%s" tag)))

;;; Rendering

(defun papis-vui--render-header (state)
  "Render the header and search row of STATE."
  (vui-vstack
   :spacing 0
   (vui-hstack
    :spacing 2
    (vui-heading-1 "Papis")
    (vui-muted
     (format "%d document%s"
             (length (papis-vui--state-documents state))
             (if (= (length (papis-vui--state-documents state)) 1) "" "s")))
    (vui-text "│" :face 'shadow)
    (vui-text "Library:" :face 'bold)
    (if (papis-vui--state-libraries state)
        (vui-list (papis-vui--state-libraries state)
                  (lambda (library)
                    (let ((active (equal library
                                         (papis-vui--state-library state))))
                      (vui-button library
                        :no-decoration t
                        :face (if active 'success 'link)
                        :help-echo (if active "Current library" "Switch library")
                        :on-click (lambda ()
                                    (papis-vui--select-library library)))))
                  #'identity
                  :vertical nil)
      (vui-muted "none")))
   (vui-newline)
   (vui-hstack
    :spacing 1
    (vui-text "Search:" :face 'bold)
    (vui-field
     :key 'papis-vui-search
     :value (papis-vui--state-input state)
     :size (max 20 (min 50 (- (papis-vui--width) 40)))
     :placeholder "e.g. author:knuth year:1984"
     :on-change (lambda (value)
                  (setf (papis-vui--state-input papis-vui--state) value))
     :on-submit (lambda (value) (papis-vui--search value)))
    (vui-button "Search"
      :on-click (lambda ()
                  (papis-vui--search
                   (or (vui-field-value 'papis-vui-search) ""))))
    (vui-button "All" :on-click (lambda () (papis-vui--search "")))
    (vui-button "Refresh" :on-click #'papis-vui-refresh))))

(defun papis-vui--render-toolbar (state)
  "Render the sort controls and result count of STATE."
  (vui-hstack
   :spacing 1
   (vui-text "Sort:" :face 'bold)
   (vui-list papis-vui-sort-fields
             (lambda (field)
               (let ((active (equal field
                                    (papis-vui--state-sort-field state))))
                 (vui-button
                  (concat field
                          (when active
                            (if (papis-vui--state-sort-reverse state) " ↓" " ↑")))
                  :no-decoration t
                  :face (if active 'success 'link)
                  :help-echo (if active "Toggle sort direction" "Sort by this field")
                  :on-click (lambda () (papis-vui--sort-by field)))))
             #'identity
             :vertical nil)
   (vui-text "  " :face 'shadow)
   (vui-muted
    (format "%d shown"
            (length (papis-vui--state-documents state))))))

(defun papis-vui--render-tags (state)
  "Render the tag buttons of STATE."
  (let* ((counts (papis-vui--tag-counts (papis-vui--state-documents state)))
         (shown (seq-take counts papis-vui-tag-limit))
         (hidden (- (length counts) (length shown))))
    (when counts
      (vui-vstack
       :spacing 0
       (vui-newline)
       (vui-hstack
        :spacing 1
        (vui-text "Tags:" :face 'bold)
        (vui-list shown
                  (lambda (pair)
                    (let ((tag (car pair)))
                      (vui-button
                       (format "%s (%d)"
                               (papis-vui--truncate tag 20)
                               (cdr pair))
                       :no-decoration t
                       :help-echo (format "Filter by tag: %s" tag)
                       :on-click (lambda ()
                                   (papis-vui--search (papis-vui--tag-query tag))))))
                  #'car
                  :vertical nil)
        (when (> hidden 0)
          (vui-muted (format "+%d more" hidden))))))))

(defun papis-vui--render-error (state)
  "Render the error pane of STATE."
  (vui-vstack
   :spacing 0
   (vui-heading-2 "Papis is not available")
   (vui-error (or (papis-vui--state-error state) "Unknown error"))
   (vui-newline)
   (vui-hstack
    :spacing 1
    (vui-button "Retry" :on-click #'papis-vui-refresh)
    (vui-button "Start server" :on-click #'papis-vui-start-server)
    (vui-button "Open server log" :on-click #'papis-open-server-log))
   (vui-newline)
   (vui-muted (format "Server: %s" (papis-server-url)))
   (vui-muted (format "Executable: %s" papis-executable))))

(defun papis-vui--render-documents (state)
  "Render the document table of STATE."
  (let ((documents (papis-vui--sorted-documents state))
        (selected (papis-vui--selected-document state))
        (title-width (max 20 (min 40 (- (papis-vui--width) 56)))))
    (if (null documents)
        (vui-muted "No documents match the current query.")
      (vui-table
       :columns (list (list :header "Title" :min-width 20)
                      (list :header "Author" :min-width 12)
                      (list :header "Year" :width 6 :align :right)
                      (list :header "Tags" :min-width 10))
       :rows
       (mapcar
        (lambda (document)
          (let* ((current (eq document selected))
                 (title (papis-document-title document))
                 (author (papis-document-author document))
                 (tags (string-join (papis-document-tags document) ", ")))
            (list
             (vui-button
              (concat (if current "▶ " "")
                      (papis-vui--truncate title title-width))
              :no-decoration t
              :face (if current 'success 'link)
              :help-echo title
              :on-click (lambda () (papis-vui--select-document document)))
             (papis-vui--truncate author 20)
             (papis-document-year-string document)
             (papis-vui--truncate tags 18))))
        documents)
       :border nil
       :sticky-header t))))

(defun papis-vui--render-metadata (document)
  "Render the metadata table of DOCUMENT."
  (let* ((keys (seq-remove (lambda (key)
                             (member key papis-vui--hidden-metadata))
                           (papis-document-keys document)))
         (keys (papis--sorted-field-keys keys))
         (value-width (max 20 (- (papis-vui--width) 22))))
    (if (null keys)
        (vui-muted "No metadata.")
      (vui-table
       :columns (list (list :header "Field" :width 12)
                      (list :header "Value" :min-width 20))
       :rows
       (mapcar
        (lambda (key)
          (let ((value (papis-document-value-string document key)))
            (list (vui-muted (papis-field-label key))
                  (vui-text (if (string-empty-p value)
                                "—"
                              (papis-vui--truncate value value-width))
                            :help-echo value))))
        keys)
       :border nil))))

(defun papis-vui--render-text-field (state document key title)
  "Render the long text field KEY of DOCUMENT with TITLE.

STATE controls whether the text is expanded or truncated."
  (let ((value (papis-document-value-string document key)))
    (unless (string-empty-p value)
      (let* ((lines (papis-vui--wrap value (max 30 (- (papis-vui--width) 4))))
             (expanded (papis-vui--state-expanded state))
             (limit papis-vui-text-lines)
             (shown (if expanded lines (seq-take lines limit))))
        (vui-vstack
         :spacing 0
         (vui-newline)
         (vui-heading-3 title)
         (vui-list shown #'vui-text)
         (when (> (length lines) limit)
           (vui-button (if expanded "Show less" "Show more")
             :no-decoration t
             :on-click
             (lambda ()
               (setf (papis-vui--state-expanded papis-vui--state)
                     (not expanded))
               (papis-vui--redraw)))))))))

(defun papis-vui--open-path (path)
  "Open PATH with `papis-open-file-function'."
  (papis-vui--call (lambda (file)
                     (funcall papis-open-file-function file))
                   path))

(defun papis-vui--render-files (document)
  "Render the attached files of DOCUMENT."
  (let ((files (papis-document-files document)))
    (if (null files)
        (vui-muted "No files attached.")
      (vui-vstack
       :spacing 0
       (vui-heading-3 "Files")
       (vui-list files
                 (lambda (file)
                   (vui-button (file-name-nondirectory file)
                     :no-decoration t
                     :help-echo file
                     :on-click (lambda () (papis-vui--open-path file))))
                 #'identity)))))

(defun papis-vui--render-actions (document)
  "Render the action buttons of DOCUMENT."
  (let ((file (papis-document-file document))
        (url (papis-document-url document))
        (directory (papis-document-directory document))
        (info (papis-document-info-file document))
        (book-files (papis-document-book-files document)))
    (vui-hstack
     :spacing 1
     (vui-button "Open"
       :disabled (null file)
       :help-echo (or file "No file attached")
       :on-click (lambda ()
                   (papis-vui--call #'papis-document-open-file document)))
     (vui-button "URL"
       :disabled (null url)
       :on-click (lambda ()
                   (papis-vui--call #'papis-document-open-url document)))
     (vui-button "Folder"
       :disabled (null directory)
       :on-click (lambda ()
                   (papis-vui--call #'papis-document-open-directory document)))
     (vui-button "Info"
       :disabled (or (null info) (not (file-exists-p info)))
       :on-click (lambda ()
                   (papis-vui--call #'papis-document-open-info document)))
     (vui-button "Edit"
       :disabled (or (null info) (not (file-exists-p info)))
       :help-echo "Manually edit the metadata and sync the book file"
       :on-click (lambda () (papis-vui-edit-metadata document)))
     (vui-button "BibTeX"
       :help-echo "Copy a BibTeX entry to the kill ring"
       :on-click (lambda () (papis-vui-copy-bibtex)))
     (vui-button "Reference"
       :help-echo "Copy a human-readable reference to the kill ring"
       :on-click (lambda () (papis-vui-copy-reference)))
     (vui-button "Auto metadata"
       :disabled (null book-files)
       :help-echo "Update EPUB/PDF metadata and rename the file"
       :on-click (lambda () (papis-vui-update-book-metadata document)))
     (vui-button "Delete"
       :face 'error
       :help-echo "Delete this document from the library"
       :on-click (lambda () (papis-vui-delete-document document))))))

(defun papis-vui--render-detail (state document)
  "Render the detail pane of STATE for DOCUMENT."
  (if (null document)
      (vui-muted "Select a document to see its details.")
    (vui-vstack
     :spacing 0
     (vui-heading-2 (papis-document-title document))
     (vui-muted (papis-document-description document))
     (vui-newline)
     (papis-vui--render-metadata document)
     (papis-vui--render-text-field state document "abstract" "Abstract")
     (papis-vui--render-text-field state document "notes" "Notes")
     (papis-vui--render-text-field state document "note" "Note")
     (vui-newline)
     (papis-vui--render-files document)
     (vui-newline)
     (papis-vui--render-actions document))))

(defun papis-vui--render-footer (state)
  "Render the footer of STATE."
  (let ((path (papis-vui--state-library-path state)))
    (vui-vstack
     :spacing 0
     (vui-newline)
     (papis-vui--rule)
     (vui-hstack
      :spacing 2
      (vui-muted
       (format "%d document%s"
               (length (papis-vui--state-documents state))
               (if (= (length (papis-vui--state-documents state)) 1) "" "s")))
      (when (papis-vui--state-library state)
        (vui-muted (format "in %s" (papis-vui--state-library state))))
      (when path
        (vui-muted (abbreviate-file-name path)))
      (vui-muted (format "server %s" (papis-server-url))))
     (vui-muted
      "g refresh   / search   l library   s sort   n/p next/prev   o open   u URL   i info   b BibTeX")
     (vui-muted
      "D delete   c copy reference   d folder   e edit metadata   m auto metadata"))))

(defun papis-vui--render (state)
  "Render the whole frontend for STATE."
  (vui-vstack
   :spacing 0
   (papis-vui--render-header state)
   (vui-newline)
   (papis-vui--render-toolbar state)
   (papis-vui--render-tags state)
   (vui-newline)
   (if (papis-vui--state-error state)
       (papis-vui--render-error state)
     (vui-vstack
      :spacing 0
      (papis-vui--render-documents state)
      (vui-newline)
      (papis-vui--render-detail state
                                (papis-vui--selected-document state))))
   (papis-vui--render-footer state)))

(vui-defcomponent papis-vui-root (state)
  "Root component of the Papis frontend."
  :render
  (papis-vui--render state))

;;; Commands

(defun papis-vui--current-document ()
  "Return the selected document, or signal a `user-error'."
  (or (and papis-vui--state
           (papis-vui--selected-document papis-vui--state))
      (user-error "No document selected")))

(defun papis-vui-refresh ()
  "Reload the current library and query."
  (interactive)
  (if (null papis-vui--state)
      (papis-vui-open)
    (papis-vui--fetch papis-vui--state
                      (papis-vui--state-library papis-vui--state)
                      (papis-vui--state-query papis-vui--state))
    (papis-vui--redraw)))

(defun papis-vui-start-server ()
  "Start the Papis server and reload the frontend."
  (interactive)
  (condition-case err
      (progn
        (papis-vui--status "starting the Papis server...")
        (papis-start-server)
        (papis-vui-refresh))
    (error
     (message "Papis: %s" (papis-error-message err))
     (when papis-vui--state
       (setf (papis-vui--state-error papis-vui--state)
             (papis-error-message err))
       (papis-vui--redraw)))))

(defun papis-vui-search (&optional query)
  "Read a Papis query and load its results.

When QUERY is non-nil, use it instead of prompting."
  (interactive)
  (papis-vui--search
   (or query
       (read-string "Papis query: "
                    (or (and papis-vui--state
                             (papis-vui--state-query papis-vui--state))
                        "")))))

(defun papis-vui-select-library (&optional library)
  "Choose a Papis library.

When LIBRARY is non-nil, switch to it instead of prompting."
  (interactive)
  (let* ((libraries (or (and papis-vui--state
                             (papis-vui--state-libraries papis-vui--state))
                        (papis-libraries)))
         (default (and papis-vui--state
                       (papis-vui--state-library papis-vui--state)))
         (library (or library
                      (completing-read "Library: " libraries nil t nil nil default))))
    (papis-vui--select-library library)))

(defun papis-vui-sort (&optional field)
  "Choose the field to sort documents by.

When FIELD is non-nil, use it instead of prompting."
  (interactive)
  (papis-vui--sort-by
   (or field
       (completing-read
        "Sort by: " papis-vui-sort-fields nil t nil nil
        (and papis-vui--state
             (papis-vui--state-sort-field papis-vui--state))))))

(defun papis-vui-next-document (&optional n)
  "Select the document N places after the current one."
  (interactive "p")
  (papis-vui--move-selection (or n 1)))

(defun papis-vui-previous-document (&optional n)
  "Select the document N places before the current one."
  (interactive "p")
  (papis-vui--move-selection (- (or n 1))))

(defun papis-vui--move-selection (n)
  "Move the selection by N documents."
  (when papis-vui--state
    (let* ((documents (papis-vui--sorted-documents papis-vui--state))
           (selected (papis-vui--selected-document papis-vui--state)))
      (if (null documents)
          (message "No documents")
        (let* ((index (or (cl-position selected documents :test #'eq) 0))
               (next (nth (mod (+ index n) (length documents)) documents)))
          (papis-vui--select-document next))))))

(defun papis-vui-open-file ()
  "Open the first file of the selected document."
  (interactive)
  (papis-vui--call #'papis-document-open-file
                   (papis-vui--current-document)))

(defun papis-vui-open-url ()
  "Open the URL of the selected document."
  (interactive)
  (papis-vui--call #'papis-document-open-url
                   (papis-vui--current-document)))

(defun papis-vui-open-directory ()
  "Open the folder of the selected document."
  (interactive)
  (papis-vui--call #'papis-document-open-directory
                   (papis-vui--current-document)))

(defun papis-vui-open-info ()
  "Open the info file of the selected document."
  (interactive)
  (papis-vui--call #'papis-document-open-info
                   (papis-vui--current-document)))

(defun papis-vui-copy-bibtex ()
  "Copy a BibTeX entry for the selected document."
  (interactive)
  (papis-vui--call
   (lambda (document)
     (let ((entry (papis-document-export document "bibtex")))
       (when (string-empty-p (string-trim entry))
         (error "Papis returned an empty BibTeX entry"))
       (kill-new entry)
       (message "Copied BibTeX for %s" (papis-document-title document))))
   (papis-vui--current-document)))

(defun papis-vui-copy-reference ()
  "Copy a human-readable reference for the selected document."
  (interactive)
  (papis-vui--call
   (lambda (document)
     (let ((reference (papis-document-citation document)))
       (kill-new reference)
       (message "Copied reference for %s" (papis-document-title document))))
   (papis-vui--current-document)))

(defun papis-vui-delete-document (&optional document)
  "Delete DOCUMENT, or the selected document, after confirmation.

The document folder is removed from disk with `papis rm'; this
cannot be undone.  The selection moves to the neighbouring
document afterwards."
  (interactive)
  (let* ((document (or document (papis-vui--current-document)))
         (title (papis-document-title document))
         (folder (papis-document-folder document))
         (documents (and papis-vui--state
                         (papis-vui--sorted-documents papis-vui--state)))
         (index (or (cl-position document documents :test #'eq) 0))
         (neighbour (or (nth (1+ index) documents)
                        (nth (1- index) documents)))
         (neighbour-id (and neighbour (papis-document-id neighbour))))
    (when (yes-or-no-p
           (format "Delete \"%s\"%s? "
                   title
                   (if folder
                       (format " (%s)" (abbreviate-file-name folder))
                     "")))
      (papis-vui--status "deleting %s..." title)
      (condition-case err
          (progn
            (papis-document-delete document)
            (message "Deleted %s" title)
            (papis-vui-refresh)
            (when (and neighbour-id papis-vui--state)
              (when-let* ((next (seq-find
                                 (lambda (candidate)
                                   (equal neighbour-id
                                          (papis-document-id candidate)))
                                 (papis-vui--state-documents papis-vui--state))))
                (papis-vui--select-document next))))
        (error
         (message "Papis: %s" (papis-error-message err)))))))

(defun papis-vui-update-book-metadata (&optional document)
  "Update the book metadata of DOCUMENT and rename its EPUB/PDF file.

Run `papis-book-metadata-script' to merge the file's metadata with
`info.yaml', write both files back, rename the file to
\"author - title (year).epub\" (or `.pdf') and refresh the Papis
cache.  PDF metadata is written with ExifTool.  When DOCUMENT has
several EPUB/PDF files, ask which one to update."
  (interactive)
  (let* ((document (or document (papis-vui--current-document)))
         (title (papis-document-title document))
         (id (papis-document-id document))
         (files (papis-document-book-files document))
         (file (cond
                ((null files) nil)
                ((null (cdr files)) (car files))
                (t (completing-read
                    (format "Book file for \"%s\": " title)
                    files nil t nil nil (car files))))))
    (unless file
      (user-error "Document \"%s\" has no EPUB or PDF file" title))
    (papis-vui--status "updating metadata for %s..." title)
    (condition-case err
        (progn
          (papis-document-update-book-metadata document file)
          (message "Updated book metadata for %s" title)
          (papis-vui-refresh)
          (when (and id papis-vui--state)
            (when-let* ((next (seq-find
                               (lambda (candidate)
                                 (equal id (papis-document-id candidate)))
                               (papis-vui--state-documents papis-vui--state))))
              (papis-vui--select-document next))))
      (error
       (message "Papis: %s" (papis-error-message err))))))

;;; Manual metadata editing

(defvar-local papis-vui--metadata-edit-document nil
  "Document whose metadata is being edited in this buffer.")

(defvar-local papis-vui--metadata-edit-file nil
  "Info file the current metadata edit buffer will be written to.")

(defun papis-vui-metadata-commit ()
  "Write the metadata buffer and sync its document.

After `info.yaml' is written, the EPUB or PDF is updated and renamed
when the document has one, and the Papis cache is refreshed.  The
buffer is killed on success and kept on error so that the metadata
can be fixed."
  (interactive)
  (let ((document papis-vui--metadata-edit-document)
        (file papis-vui--metadata-edit-file))
    (unless (and document file)
      (user-error "This is not a Papis metadata edit buffer"))
    (unless (string-match-p "\\S-" (buffer-substring-no-properties
                                    (point-min) (point-max)))
      (user-error "Refusing to write an empty metadata file"))
    (let ((id (papis-document-id document))
          (title (papis-document-title document)))
      (condition-case err
          (progn
            (write-region (point-min) (point-max) file nil 'silent)
            (papis-vui--status "syncing metadata for %s..." title)
            (papis-document-sync-metadata document)
            (message "Updated metadata for %s" title)
            (papis-vui-refresh)
            (when (and id papis-vui--state)
              (when-let* ((next (seq-find
                                 (lambda (candidate)
                                   (equal id (papis-document-id candidate)))
                                 (papis-vui--state-documents papis-vui--state))))
                (papis-vui--select-document next)))
            (kill-buffer (current-buffer)))
        (error
         (message "Papis: %s" (papis-error-message err)))))))

(defun papis-vui-metadata-cancel ()
  "Close the metadata edit buffer, asking before discarding edits."
  (interactive)
  (when (or (not (buffer-modified-p))
            (yes-or-no-p "Discard metadata edits? "))
    (kill-buffer (current-buffer))))

(defvar-keymap papis-vui-metadata-edit-mode-map
  :parent text-mode-map
  "C-c C-c" #'papis-vui-metadata-commit
  "C-c C-k" #'papis-vui-metadata-cancel)

(define-derived-mode papis-vui-metadata-edit-mode text-mode "Papis-Metadata"
  "Major mode for manually editing a Papis document's metadata.

\{papis-vui-metadata-edit-mode-map}"
  (setq-local header-line-format
              "Edit metadata: C-c C-c apply and sync, C-c C-k cancel")
  (setq-local truncate-lines nil))

(defun papis-vui-edit-metadata (&optional document)
  "Manually edit the metadata of DOCUMENT.

Open DOCUMENT's `info.yaml' in a dedicated buffer.  Press
\[papis-vui-metadata-commit] to write the changes, update and rename
the EPUB or PDF when there is one, refresh the Papis cache and
return to the frontend.  Press \[papis-vui-metadata-cancel] to
cancel."
  (interactive)
  (let* ((document (or document (papis-vui--current-document)))
         (info (papis-document-info-file document))
         (title (papis-document-title document)))
    (unless (and info (file-exists-p info))
      (user-error "Document \"%s\" has no info file" title))
    (let ((buffer (generate-new-buffer
                   (format "*Papis metadata: %s*" title))))
      (with-current-buffer buffer
        (insert-file-contents info)
        (papis-vui-metadata-edit-mode)
        (setq-local papis-vui--metadata-edit-document document
                    papis-vui--metadata-edit-file info)
        (set-buffer-modified-p nil)
        (goto-char (point-min)))
      (pop-to-buffer buffer)
      (message "Edit metadata, then C-c C-c to apply and sync"))))

;;; Major mode and entry point

(defvar-keymap papis-vui-mode-map
  :parent vui-mode-map
  "g" #'papis-vui-refresh
  "r" #'papis-vui-refresh
  "/" #'papis-vui-search
  "l" #'papis-vui-select-library
  "s" #'papis-vui-sort
  "n" #'papis-vui-next-document
  "p" #'papis-vui-previous-document
  "o" #'papis-vui-open-file
  "u" #'papis-vui-open-url
  "d" #'papis-vui-open-directory
  "i" #'papis-vui-open-info
  "b" #'papis-vui-copy-bibtex
  "c" #'papis-vui-copy-reference
  "e" #'papis-vui-edit-metadata
  "m" #'papis-vui-update-book-metadata
  "D" #'papis-vui-delete-document)

(defun papis-vui--revert (&rest _)
  "Revert the Papis frontend buffer."
  (papis-vui-refresh))

(define-derived-mode papis-vui-mode vui-mode "Papis"
  "Major mode for the Papis VUI frontend.

\\{papis-vui-mode-map}"
  :group 'papis-vui
  (setq-local truncate-lines nil)
  (setq-local revert-buffer-function #'papis-vui--revert))

;;;###autoload
(defun papis-vui-open (&optional library query)
  "Open the Papis frontend.

With LIBRARY and QUERY, start with that library and Papis query."
  (interactive)
  (papis-vui--status "connecting to Papis...")
  (let ((buffer (get-buffer-create papis-vui-buffer-name)))
    (with-current-buffer buffer
      (papis-vui-mode)
      (setq papis-vui--state (papis-vui--initial-state library query)))
    (setq papis-vui--instance
          (vui-mount (vui-component 'papis-vui-root
                                    :state papis-vui--state)
                     papis-vui-buffer-name))
    (unless papis-vui--instance
      (message "Papis: could not mount the frontend"))
    papis-vui--instance))

;;;###autoload
(defun papis-vui (&optional library query)
  "Open the Papis frontend.

With LIBRARY and QUERY, start with that library and Papis query."
  (interactive)
  (papis-vui-open library query))

(provide 'papis-vui)
;;; papis-vui.el ends here
