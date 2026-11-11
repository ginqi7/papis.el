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

(defun papis-test--document (data &optional library)
  "Create a document from DATA for tests."
  (car (papis--parse-documents (list data) (or library "books"))))

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
        (expect (papis-document-file-type document) :to-equal "PDF")
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

    (it "reports no file type without an attached extension"
      (let ((document (papis-test--document
                       '((title . "No file")
                         (files . ("/tmp/README"))))))
        (expect (papis-document-file-type document) :to-be nil)))

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

  (describe "tagging"
    (it "offers the preset tags"
      (expect papis-tags :to-equal '("reading" "read")))

    (it "builds `papis tag' arguments from the document folder"
      (let ((document (papis-test--document
                       '((papis_id . "abc123")
                         (title . "A Title")
                         (_papis_local_folder . "/tmp/books/a-title"))
                       "books")))
        (expect (papis-document-tag-arguments document "--add" "reading")
                :to-equal '("-l" "books" "tag" "--batch" "--all"
                            "--doc-folder" "/tmp/books/a-title/"
                            "--add" "reading"))))

    (it "falls back to the document id"
      (let ((document (papis-test--document
                       '((papis_id . "abc123")
                         (title . "A Title"))
                       "books")))
        (expect (papis-document-tag-arguments document "--remove" "read")
                :to-equal '("-l" "books" "tag" "--batch" "--all"
                            "papis_id:abc123"
                            "--remove" "read"))))

    (it "signals an error without an id or folder"
      (let ((document (papis-test--document '((title . "A Title")) "books")))
        (expect (papis-document-tag-arguments document "--add" "reading")
                :to-throw 'papis-error)))

    (it "adds a tag and updates the document in memory"
      (let ((document (papis-test--document
                       '((papis_id . "abc123")
                         (title . "A Title")
                         (tags . ("old")))
                       "books"))
            (arguments nil))
        (cl-letf (((symbol-function 'papis--call-cli-result)
                   (lambda (args)
                     (setq arguments args)
                     (list :status 0 :stdout "" :stderr ""))))
          (expect (papis-document-tag-add document "reading") :to-be-truthy)
          (expect arguments
                  :to-equal '("-l" "books" "tag" "--batch" "--all"
                              "papis_id:abc123"
                              "--add" "reading"))
          (expect (papis-document-tags document)
                  :to-equal '("old" "reading")))))

    (it "does not duplicate a tag that is already there"
      (let ((document (papis-test--document
                       '((papis_id . "abc123")
                         (title . "A Title")
                         (tags . ("reading")))
                       "books")))
        (cl-letf (((symbol-function 'papis--call-cli-result)
                   (lambda (_arguments)
                     (list :status 0 :stdout "" :stderr ""))))
          (papis-document-tag-add document "reading")
          (expect (papis-document-tags document) :to-equal '("reading")))))

    (it "removes a tag from the document in memory"
      (let ((document (papis-test--document
                       '((papis_id . "abc123")
                         (title . "A Title")
                         (tags . ("old" "reading" "read")))
                       "books")))
        (cl-letf (((symbol-function 'papis--call-cli-result)
                   (lambda (_arguments)
                     (list :status 0 :stdout "" :stderr ""))))
          (expect (papis-document-tag-remove document "reading") :to-be-truthy)
          (expect (papis-document-tags document)
                  :to-equal '("old" "read")))))

    (it "reports failures from the tag command"
      (let ((document (papis-test--document
                       '((papis_id . "abc123")
                         (title . "A Title")
                         (tags . ("old")))
                       "books")))
        (cl-letf (((symbol-function 'papis--call-cli-result)
                   (lambda (_arguments)
                     (list :status 1 :stdout ""
                           :stderr "'tags' field does not contain 'missing'\n"))))
          (expect (papis-document-tag-remove document "missing")
                  :to-throw 'papis-error)
          (expect (papis-document-tags document) :to-equal '("old")))))

    (it "restarts only a server started by this Emacs"
      (cl-letf (((symbol-function 'process-live-p)
                 (lambda (_process) nil))
                ((symbol-function 'papis-restart-server)
                 (lambda () (error "must not restart"))))
        (expect (papis-document-restart-server) :to-be nil))
      (cl-letf (((symbol-function 'process-live-p)
                 (lambda (_process) t))
                ((symbol-function 'papis-restart-server)
                 (lambda () 'restarted)))
        (expect (papis-document-restart-server) :to-be-truthy))))

  (describe "book metadata"
    (it "finds the EPUB and PDF files of a document"
      (let ((document (papis-test--document
                       '((title . "A Book")
                         (files . ("/tmp/a.pdf" "/tmp/b.EPUB" "/tmp/c.epub"
                                   "/tmp/d.PDF" "/tmp/e.djvu")))
                       "books")))
        (expect (papis-document-book-files document)
                :to-equal '("/tmp/a.pdf" "/tmp/b.EPUB" "/tmp/c.epub"
                            "/tmp/d.PDF"))))

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
          (delete-directory folder t))))

    (it "updates the metadata of a PDF and then the cache"
      (let* ((folder (make-temp-file "papis-test-doc-" t))
             (file (expand-file-name "book.pdf" folder))
             (document (papis-test--document
                        `((title . "A Book")
                          (files . (,file))
                          (_papis_local_folder . ,folder))
                        "books"))
             (ran nil)
             (refreshed nil))
        (unwind-protect
            (progn
              (with-temp-file file (insert "fake pdf"))
              (cl-letf (((symbol-function 'papis--run-book-metadata-script)
                         (lambda (target)
                           (setq ran target)
                           "renamed: book.pdf -> a-book.pdf"))
                        ((symbol-function 'papis-document-refresh-cache)
                         (lambda (doc)
                           (setq refreshed doc)
                           (list :status 0))))
                (expect (papis-document-update-book-metadata document)
                        :to-equal "renamed: book.pdf -> a-book.pdf")
                (expect ran :to-equal file)
                (expect refreshed :to-be document)))
          (delete-directory folder t)))))

  (describe "manual metadata editing"
    (it "syncs an EPUB or PDF after an edit and otherwise refreshes the cache"
      (let* ((epub (papis-test--document
                    '((title . "A Book")
                      (files . ("/tmp/book.epub")))
                    "books"))
             (pdf (papis-test--document
                   '((title . "A Paper")
                     (files . ("/tmp/paper.pdf")))
                   "books"))
             (djvu (papis-test--document
                    '((title . "A Scan")
                      (files . ("/tmp/scan.djvu")))
                    "books"))
             (synced nil)
             (refreshed nil))
        (cl-letf (((symbol-function 'papis-document-update-book-metadata)
                   (lambda (doc)
                     (setq synced doc)
                     (setq refreshed nil)
                     'book))
                  ((symbol-function 'papis-document-refresh-cache)
                   (lambda (doc)
                     (setq refreshed doc)
                     'cache)))
          (expect (papis-document-sync-metadata epub) :to-equal 'book)
          (expect synced :to-be epub)
          (expect refreshed :to-be nil)
          (expect (papis-document-sync-metadata pdf) :to-equal 'book)
          (expect synced :to-be pdf)
          (expect refreshed :to-be nil)
          (expect (papis-document-sync-metadata djvu) :to-equal 'cache)
          (expect refreshed :to-be djvu)))))

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
