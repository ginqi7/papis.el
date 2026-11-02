;;; papis-test.el --- Tests for the Papis API client  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; These tests cover the parts of papis.el that do not need a running
;; Papis server: JSON parsing, document accessors, query paths, sorting
;; and cleanup of CLI output.

;;; Code:

(require 'buttercup)
(require 'papis)
(require 'papis-vui)

(defun papis-test--document (data &optional library)
  "Create a document from DATA for tests."
  (car (papis--parse-documents (list data) (or library "books"))))

(defun papis-test--state (documents)
  "Create a frontend state holding DOCUMENTS."
  (papis-vui--state-create
   :libraries '("books" "papers")
   :library "books"
   :query ""
   :input ""
   :documents documents
   :sort-field "title"
   :sort-reverse nil))

(describe "papis.el"
  (describe "documents"
    (it "reads metadata with string, symbol and keyword keys"
      (let ((document (papis-test--document
                       '((title . "Structure and Interpretation")
                         (author . "Abelson, Harold")
                         (year . 1985)))))
        (expect (papis-document-get document "title")
                :to-equal "Structure and Interpretation")
        (expect (papis-document-get document 'author)
                :to-equal "Abelson, Harold")
        (expect (papis-document-get document :year)
                :to-equal 1985)
        (expect (papis-document-get document "missing" "fallback")
                :to-equal "fallback")))

    (it "exposes the usual fields"
      (let ((document (papis-test--document
                       '((papis_id . "abc123")
                         (title . "A Title")
                         (author . ("A" "B"))
                         (year . 2020)
                         (files . ("/tmp/a.pdf"))
                         (tags . ("lisp" "emacs"))))))
        (expect (papis-document-id document) :to-equal "abc123")
        (expect (papis-document-title document) :to-equal "A Title")
        (expect (papis-document-author document) :to-equal "A, B")
        (expect (papis-document-year-string document) :to-equal "2020")
        (expect (papis-document-file document) :to-equal "/tmp/a.pdf")
        (expect (papis-document-directory document) :to-equal "/tmp/")
        (expect (papis-document-tags document) :to-equal '("lisp" "emacs"))))

    (it "parses tags stored as a string"
      (let ((document (papis-test--document
                       '((tags . "lisp, emacs; text")))))
        (expect (papis-document-tags document)
                :to-equal '("lisp" "emacs" "text"))))

    (it "falls back to the ref for the id"
      (let ((document (papis-test--document '((ref . "knuth84")))))
        (expect (papis-document-id document) :to-equal "knuth84")))

    (it "orders fields for display"
      (let ((document (papis-test--document
                       '((year . 1985)
                         (title . "SICP")
                         (author . "Abelson")
                         (publisher . "MIT")))))
        (expect (mapcar #'car (papis-document-fields document))
                :to-equal '("title" "author" "year" "publisher")))))

  (describe "queries"
    (it "builds the all-documents path"
      (expect (papis--document-path "books" "")
              :to-equal "/api/library/books/document"))

    (it "encodes query characters"
      (expect (papis--document-path "books" "author:knuth year:1984")
              :to-equal
              "/api/library/books/document/author%3Aknuth%20year%3A1984"))

    (it "encodes library names"
      (expect (papis--document-path "my books" "")
              :to-equal "/api/library/my%20books/document")))

  (describe "deleting"
    (it "builds `papis rm' arguments from the document id"
      (let ((document (papis-test--document
                       '((papis_id . "abc123")
                         (title . "A Title"))
                       "books")))
        (expect (papis-document-delete-arguments document)
                :to-equal '("-l" "books" "rm" "--force" "--all"
                            "papis_id:abc123"))))

    (it "prefers the document folder"
      (let ((document (papis-test--document
                       '((papis_id . "abc123")
                         (title . "A Title")
                         (_papis_local_folder . "/tmp/books/a-title"))
                       "books")))
        (expect (papis-document-delete-arguments document)
                :to-equal '("-l" "books" "rm" "--force" "--all"
                            "--doc-folder" "/tmp/books/a-title/"))))

    (it "uses the file directory only when it holds an info file"
      (let* ((folder (make-temp-file "papis-test-doc-" t))
             (info (expand-file-name papis-info-file-name folder))
             (document (papis-test--document
                        `((title . "A Title")
                          (files . (,(expand-file-name "a-title.pdf" folder))))
                        "books")))
        (unwind-protect
            (progn
              (expect (papis-document-folder document) :to-be nil)
              (with-temp-file info (insert "title: A Title\n"))
              (expect (papis-document-folder document)
                      :to-equal (file-name-as-directory folder)))
          (delete-directory folder t))))

    (it "signals an error without an id or folder"
      (let ((document (papis-test--document '((title . "A Title")) "books")))
        (expect (papis-document-delete-arguments document)
                :to-throw 'papis-error)))

    (it "detects that `papis rm' deleted nothing"
      (let* ((folder (make-temp-file "papis-test-doc-" t))
             (document (papis-test--document
                        `((title . "A Title")
                          (_papis_local_folder . ,folder))
                        "books")))
        (unwind-protect
            (cl-letf (((symbol-function 'papis--call-cli-result)
                       (lambda (_arguments)
                         (list :status 0 :stdout ""
                               :stderr "[WARNING] commands.rm: No documents retrieved\n"))))
              (expect (papis-document-delete document) :to-throw 'papis-error))
          (delete-directory folder t))))

    (it "succeeds when the folder is gone afterwards"
      (let* ((folder (make-temp-file "papis-test-doc-" t))
             (document (papis-test--document
                        `((title . "A Title")
                          (_papis_local_folder . ,folder))
                        "books")))
        (cl-letf (((symbol-function 'papis--call-cli-result)
                   (lambda (_arguments)
                     (delete-directory folder t)
                     (list :status 0 :stdout "" :stderr ""))))
          (expect (papis-document-delete document) :to-be-truthy)))))

  (describe "book metadata"
    (it "finds the EPUB files of a document"
      (let ((document (papis-test--document
                       '((title . "A Book")
                         (files . ("/tmp/a.pdf" "/tmp/b.EPUB" "/tmp/c.epub")))
                       "books")))
        (expect (papis-document-book-files document)
                :to-equal '("/tmp/b.EPUB" "/tmp/c.epub"))))

    (it "runs an executable metadata script directly"
      (let ((script (make-temp-file "papis-book-metadata-" nil ".py")))
        (unwind-protect
            (progn
              (set-file-modes script #o755)
              (let ((papis-book-metadata-script script))
                (expect (papis-document-book-metadata-command "/tmp/book.epub")
                        :to-equal (list script "/tmp/book.epub"))))
          (delete-file script))))

    (it "refreshes the cache for the document folder"
      (let ((document (papis-test--document
                       '((title . "A Book")
                         (_papis_local_folder . "/tmp/books/a-book"))
                       "books"))
            (arguments nil))
        (cl-letf (((symbol-function 'papis--call-cli-result)
                   (lambda (args)
                     (setq arguments args)
                     (list :status 0 :stdout "" :stderr ""))))
          (papis-document-refresh-cache document)
          (expect arguments
                  :to-equal '("-l" "books" "cache" "update" "--all"
                              "--doc-folder" "/tmp/books/a-book/")))))

    (it "updates the book metadata and then the cache"
      (let* ((folder (make-temp-file "papis-test-doc-" t))
             (file (expand-file-name "book.epub" folder))
             (document (papis-test--document
                        `((title . "A Book")
                          (files . (,file))
                          (_papis_local_folder . ,folder))
                        "books"))
             (ran nil)
             (refreshed nil))
        (unwind-protect
            (progn
              (with-temp-file file (insert "fake epub"))
              (cl-letf (((symbol-function 'papis--run-book-metadata-script)
                         (lambda (target)
                           (setq ran target)
                           "renamed: book.epub -> a-book.epub"))
                        ((symbol-function 'papis-document-refresh-cache)
                         (lambda (doc)
                           (setq refreshed doc)
                           (list :status 0))))
                (expect (papis-document-update-book-metadata document)
                        :to-equal "renamed: book.epub -> a-book.epub")
                (expect ran :to-equal file)
                (expect refreshed :to-be document)))
          (delete-directory folder t)))))

  (describe "manual metadata editing"
    (it "syncs an EPUB after an edit and otherwise refreshes the cache"
      (let* ((epub (papis-test--document
                    '((title . "A Book")
                      (files . ("/tmp/book.epub")))
                    "books"))
             (pdf (papis-test--document
                   '((title . "A Paper")
                     (files . ("/tmp/paper.pdf")))
                   "books"))
             (synced nil)
             (refreshed nil))
        (cl-letf (((symbol-function 'papis-document-update-book-metadata)
                   (lambda (doc)
                     (setq synced doc)
                     'book))
                  ((symbol-function 'papis-document-refresh-cache)
                   (lambda (doc)
                     (setq refreshed doc)
                     'cache)))
          (expect (papis-document-sync-metadata epub) :to-equal 'book)
          (expect synced :to-be epub)
          (expect refreshed :to-be nil)
          (expect (papis-document-sync-metadata pdf) :to-equal 'cache)
          (expect refreshed :to-be pdf))))

    (it "opens an editable metadata buffer"
      (let* ((folder (make-temp-file "papis-test-doc-" t))
             (info (expand-file-name "info.yaml" folder))
             (document (papis-test--document
                        `((title . "A Book")
                          (_papis_local_folder . ,folder))
                        "books"))
             (opened nil))
        (unwind-protect
            (progn
              (with-temp-file info
                (insert "title: A Book\nauthor: A Writer\n"))
              (cl-letf (((symbol-function 'pop-to-buffer)
                         (lambda (buffer) (setq opened buffer))))
                (let ((inhibit-message t))
                  (papis-vui-edit-metadata document)))
              (expect opened :to-be-truthy)
              (with-current-buffer opened
                (expect major-mode :to-be 'papis-vui-metadata-edit-mode)
                (expect (buffer-string)
                        :to-equal "title: A Book\nauthor: A Writer\n")
                (expect papis-vui--metadata-edit-document :to-be document)
                (expect papis-vui--metadata-edit-file :to-equal info))
              (kill-buffer opened))
          (delete-directory folder t))))

    (it "writes the edit buffer and syncs the document on commit"
      (let* ((folder (make-temp-file "papis-test-doc-" t))
             (info (expand-file-name "info.yaml" folder))
             (document (papis-test--document
                        `((papis_id . "abc123")
                          (title . "A Book")
                          (_papis_local_folder . ,folder))
                        "books"))
             (buffer (generate-new-buffer "*papis-test-metadata*"))
             (synced nil)
             (refreshed nil))
        (unwind-protect
            (progn
              (with-temp-file info (insert "title: Old Title\n"))
              (with-current-buffer buffer
                (insert "title: New Title\nauthor: New Author\n")
                (papis-vui-metadata-edit-mode)
                (setq papis-vui--metadata-edit-document document
                      papis-vui--metadata-edit-file info)
                (cl-letf (((symbol-function 'papis-document-sync-metadata)
                           (lambda (doc) (setq synced doc)))
                          ((symbol-function 'papis-vui-refresh)
                           (lambda () (setq refreshed t))))
                  (let ((inhibit-message t))
                    (papis-vui-metadata-commit))))
              (expect (with-temp-buffer
                        (insert-file-contents info)
                        (buffer-string))
                      :to-equal "title: New Title\nauthor: New Author\n")
              (expect synced :to-be document)
              (expect refreshed :to-be t)
              (expect (buffer-live-p buffer) :to-be nil))
          (when (buffer-live-p buffer) (kill-buffer buffer))
          (delete-directory folder t)))))

  (describe "sorting"
    (let* ((a (papis-test--document
               '((title . "Alpha") (author . "Zed") (year . 2000))))
           (b (papis-test--document
               '((title . "Beta") (author . "Amy") (year . 1990))))
           (c (papis-test--document
               '((title . "Gamma") (author . "Bob")))))
      (it "sorts by title"
        (let* ((state (papis-test--state (list b c a)))
               (titles (mapcar #'papis-document-title
                               (papis-vui--sorted-documents state))))
          (expect titles :to-equal '("Alpha" "Beta" "Gamma"))))

      (it "sorts numbers numerically and missing values last"
        (let* ((state (papis-test--state (list a b c)))
               (titles (progn
                         (setf (papis-vui--state-sort-field state) "year")
                         (mapcar #'papis-document-title
                                 (papis-vui--sorted-documents state)))))
          (expect titles :to-equal '("Beta" "Alpha" "Gamma"))))

      (it "reverses the comparison"
        (let* ((state (papis-test--state (list a b c))))
          (setf (papis-vui--state-sort-field state) "title"
                (papis-vui--state-sort-reverse state) t)
          (expect (mapcar #'papis-document-title
                          (papis-vui--sorted-documents state))
                  :to-equal '("Gamma" "Beta" "Alpha"))))

      (it "keeps missing values last when sorting in reverse"
        (let* ((state (papis-test--state (list a b c))))
          (setf (papis-vui--state-sort-field state) "year"
                (papis-vui--state-sort-reverse state) t)
          (expect (mapcar #'papis-document-title
                          (papis-vui--sorted-documents state))
                  :to-equal '("Alpha" "Beta" "Gamma"))))))

  (describe "errors and CLI output"
    (it "prints a custom error without the data quotes"
      (expect (papis-error-message
               '(papis-server-error "server is down"))
              :to-equal "server is down"))

    (it "removes Papis warnings and log lines from CLI output"
      (expect (papis--clean-cli-output
               (string-join
                '("The configuration is loaded from '/tmp/papis'."
                  "[INFO] commands.export: Dumping to STDOUT."
                  "@book{sicp,"
                  "  title = {SICP}"
                  "}")
                "\n"))
              :to-equal "@book{sicp,\n  title = {SICP}\n}"))))

;;; papis-test.el ends here
