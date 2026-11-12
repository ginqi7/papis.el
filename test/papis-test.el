;;; papis-test.el --- Tests for the Papis API client  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; These tests cover the parts of papis.el that do not need a running
;; Papis executable: JSON parsing, document accessors, sorting and
;; cleanup of CLI output.

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

  (describe "CLI backend"
    (it "lists libraries from `papis list --libraries'"
      (cl-letf (((symbol-function 'papis--call-cli)
                 (lambda (arguments)
                   (expect arguments :to-equal '("list" "--libraries"))
                   "books /tmp/books\npapers /tmp/papers\n")))
        (expect (papis--libraries-via-cli)
                :to-equal '("books" "papers"))))

    (it "returns the path of a library"
      (cl-letf (((symbol-function 'papis--call-cli)
                 (lambda (_arguments)
                   "books /tmp/books\npapers /tmp/papers\n")))
        (expect (papis--library-via-cli "papers")
                :to-equal '(:name "papers" :path "/tmp/papers"))))

    (it "uses the CLI for documents"
      (cl-letf (((symbol-function 'papis--documents-via-python)
                 (lambda (library query) (list library query))))
        (expect (papis-documents "books" "author:knuth")
                :to-equal '("books" "author:knuth"))))

    (it "uses the CLI for exports"
      (cl-letf (((symbol-function 'papis--call-cli)
                 (lambda (arguments) (format "%S" arguments))))
        (expect (papis-document-format "books" "papis_id:abc" "json")
                :to-equal
                "(\"-l\" \"books\" \"export\" \"--batch\" \"--all\" \"-f\" \"json\" \"papis_id:abc\")"))))

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
            (arguments nil)
            (refreshed nil))
        (cl-letf (((symbol-function 'papis-document-refresh-cache)
                   (lambda (doc) (setq refreshed doc)))
                  ((symbol-function 'papis--call-cli-result)
                   (lambda (args)
                     (setq arguments args)
                     (list :status 0 :stdout "" :stderr ""))))
          (expect (papis-document-tag-add document "reading") :to-be-truthy)
          (expect arguments
                  :to-equal '("-l" "books" "tag" "--batch" "--all"
                              "papis_id:abc123"
                              "--add" "reading"))
          (expect (papis-document-tags document)
                  :to-equal '("old" "reading"))
          (expect refreshed :to-be document))))

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
          (expect (papis-document-tags document) :to-equal '("old"))))))

  (describe "metadata updates"
    (it "reads an ISBN from a string or a list"
      (let ((document (papis-test--document
                       '((title . "A Book")
                         (isbn . ("0262560992" "9780262560993"))))))
        (expect (papis-document-isbn document) :to-equal "0262560992"))
      (expect (papis-document-isbn
               (papis-test--document '((title . "A Book")
                                       (isbn . "9780262560993"))))
              :to-equal "9780262560993")
      (expect (papis-document-isbn
               (papis-test--document '((title . "A Book"))))
              :to-be nil))

    (it "builds `papis update' arguments from the document folder"
      (let ((document (papis-test--document
                       '((papis_id . "abc123")
                         (title . "A Title")
                         (_papis_local_folder . "/tmp/books/a-title")))))
        (expect (papis-document-update-arguments
                 document
                 (list (cons "title" "New Title")
                       (cons 'year "2000")))
                :to-equal '("-l" "books" "update" "--batch" "--all"
                            "--doc-folder" "/tmp/books/a-title/"
                            "--set" "title" "New Title"
                            "--set" "year" "2000"))))

    (it "falls back to the document id"
      (let ((document (papis-test--document
                       '((papis_id . "abc123")
                         (title . "A Title")))))
        (expect (papis-document-update-arguments
                 document (list (cons "title" "A Title")))
                :to-equal '("-l" "books" "update" "--batch" "--all"
                            "papis_id:abc123"
                            "--set" "title" "A Title"))))

    (it "updates the document in memory and skips empty values"
      (let ((document (papis-test--document
                       '((papis_id . "abc123")
                         (title . "Old Title")
                         (year . 1990))))
            (arguments nil)
            (refreshed nil))
        (cl-letf (((symbol-function 'papis-document-refresh-cache)
                   (lambda (doc) (setq refreshed doc)))
                  ((symbol-function 'papis--call-cli-result)
                   (lambda (args)
                     (setq arguments args)
                     (list :status 0 :stdout "" :stderr ""))))
          (papis-document-update
           document (list (cons "title" "New Title")
                          (cons "author_list"
                                (list (list (cons "family" "Writer")
                                            (cons "given" "A."))))
                          (cons "publisher" nil)))
          (expect arguments
                  :to-equal '("-l" "books" "update" "--batch" "--all"
                              "papis_id:abc123"
                              "--set" "title" "New Title"
                              "--set" "author_list"
                              "[{\"family\":\"Writer\",\"given\":\"A.\"}]"
                              "--reset" "author"))
          (expect (papis-document-title document) :to-equal "New Title")
          (expect (papis-document-get document "author_list")
                  :to-equal '((("family" . "Writer") ("given" . "A."))))
          (expect (papis-document-year document) :to-equal 1990)
          (expect refreshed :to-be document))))

    (it "regenerates author from author_list with Papis"
      (let ((document (papis-test--document
                       '((papis_id . "abc123")
                         (title . "A Title")
                         (_papis_local_folder . "/tmp/books/a-title"))))
            (arguments nil)
            (refreshed nil))
        (cl-letf (((symbol-function 'papis-document-refresh-cache)
                   (lambda (doc) (setq refreshed doc)))
                  ((symbol-function 'papis--call-cli-result)
                   (lambda (args)
                     (setq arguments args)
                     (list :status 0 :stdout "" :stderr "")))
                  ((symbol-function 'papis--call-cli)
                   (lambda (_args) "Writer, A."))
                  ((symbol-function 'papis--clean-cli-output) #'identity))
          (expect (papis-document-reset-author document) :to-be-truthy)
          (expect arguments
                  :to-equal (list "-l" "books" "exec" papis-author-script
                                  "/tmp/books/a-title/"))
          (expect (papis-document-author document) :to-equal "Writer, A.")
          (expect refreshed :to-be document))))

    (it "builds `papis update' arguments that append a file"
      (let ((document (papis-test--document
                       '((papis_id . "abc123")
                         (title . "A Title")
                         (_papis_local_folder . "/tmp/books/a-title")))))
        (expect (papis-document-add-file-arguments document "cover.jpg")
                :to-equal '("-l" "books" "update" "--batch" "--all"
                            "--doc-folder" "/tmp/books/a-title/"
                            "--append" "files" "cover.jpg"))))

    (it "attaches a file and updates the document in memory"
      (let ((document (papis-test--document
                       '((papis_id . "abc123")
                         (title . "A Title")
                         (files . ("book.pdf"))
                         (_papis_local_folder . "/tmp/books/a-title"))))
            (arguments nil)
            (refreshed nil))
        (cl-letf (((symbol-function 'papis-document-refresh-cache)
                   (lambda (doc) (setq refreshed doc)))
                  ((symbol-function 'papis--call-cli-result)
                   (lambda (args)
                     (setq arguments args)
                     (list :status 0 :stdout "" :stderr ""))))
          (expect (papis-document-add-file document "cover.jpg")
                  :to-equal "cover.jpg")
          (expect arguments
                  :to-equal '("-l" "books" "update" "--batch" "--all"
                              "--doc-folder" "/tmp/books/a-title/"
                              "--append" "files" "cover.jpg"))
          (expect (papis-document-files document)
                  :to-equal '("book.pdf" "cover.jpg"))
          (expect refreshed :to-be document))))

    (it "copies a document without sharing metadata"
      (let* ((original (papis-test--document
                        '((title . "Old Title") (tags . ("a")))))
             (copy (papis-document-copy original)))
        (papis--document-set-field copy "title" "New Title")
        (expect (papis-document-title original) :to-equal "Old Title")
        (expect (papis-document-title copy) :to-equal "New Title"))))

  (describe "adding books"
    (it "parses author lists"
      (expect (papis-author-list-from-string "Alex Xu and Sahn Lam")
              :to-equal
              '((("family" . "Xu") ("given" . "Alex"))
                (("family" . "Lam") ("given" . "Sahn"))))
      (expect (papis-author-list-from-string
               "Kleppmann, Martin; Chris Riccomini;")
              :to-equal
              '((("family" . "Kleppmann") ("given" . "Martin"))
                (("family" . "Riccomini") ("given" . "Chris")))))

    (it "parses defaults from a book file name"
      (expect (papis--book-name-defaults
               "/tmp/Alex Xu - System Design Interview (2020).epub")
              :to-equal '(:author "Alex Xu"
                          :title "System Design Interview"
                          :year "2020"))
      (expect (papis--book-name-defaults
               "/tmp/Derek Sivers - Derek Sivers Blog.epub")
              :to-equal '(:author "Derek Sivers"
                          :title "Derek Sivers Blog"
                          :year nil)))

    (it "builds `papis add' arguments"
      (expect (papis-add-book-arguments "books" "/tmp/book.epub" "A Title")
              :to-equal '("-l" "books" "add" "--batch" "--no-confirm"
                          "--set" "title" "A Title" "/tmp/book.epub")))

    (it "adds a book and writes structured metadata"
      (let* ((folder (make-temp-file "papis-add-book-" t))
             (file (expand-file-name "Alex Xu - Test Book (2020).epub"
                                     folder))
             (arguments nil)
             (document nil)
             (fields nil))
        (unwind-protect
            (progn
              (write-region "" nil file)
              (cl-letf (((symbol-function 'read-string)
                         (lambda (_prompt &optional initial)
                           (or initial "")))
                        ((symbol-function 'papis--call-cli-result)
                         (lambda (args)
                           (setq arguments args)
                           (list :status 0 :stdout ""
                                 :stderr "Document folder is '/tmp/new-book'.")))
                        ((symbol-function 'papis-document-update)
                         (lambda (doc update-fields)
                           (setq document doc)
                           (setq fields update-fields))))
                (expect (papis-add-book file "books")
                        :to-equal "/tmp/new-book")
                (expect arguments
                        :to-equal (papis-add-book-arguments
                                   "books" file "Test Book"))
                (expect (papis-document-get document "_papis_local_folder")
                        :to-equal "/tmp/new-book")
                (expect fields
                        :to-equal
                        '(("title" . "Test Book")
                          ("year" . "2020")
                          ("author_list"
                           (("family" . "Xu") ("given" . "Alex")))))))
          (delete-directory folder t)))))

  (describe "paper metadata"
    (it "builds paper search arguments"
      (expect (papis--paper-search-arguments
               "papers" "crossref" "attention" 5 "/tmp/out.json")
              :to-equal '("-l" "papers" "explore" "crossref"
                          "-q" "attention" "-m" "5"
                          "export" "--format" "json"
                          "--out" "/tmp/out.json")))

    (it "labels paper results"
      (let ((document (make-hash-table :test #'equal)))
        (puthash "title" "Attention is all you need" document)
        (puthash "author" "Vaswani, Ashish" document)
        (puthash "year" "2017" document)
        (expect (papis--paper-label document 0)
                :to-equal "1. Attention is all you need — Vaswani, Ashish (2017)")))

    (it "maps paper metadata to Papis fields"
      (let ((document (make-hash-table :test #'equal))
            (author (make-hash-table :test #'equal)))
        (puthash "title" "Attention is all you need" document)
        (puthash "year" "2017" document)
        (puthash "doi" "10.1/example" document)
        (puthash "family" "Vaswani" author)
        (puthash "given" "Ashish" author)
        (puthash "author_list" (list author) document)
        (expect (papis--paper-metadata document)
                :to-equal
                '(("title" . "Attention is all you need")
                  ("author_list"
                   (("family" . "Vaswani") ("given" . "Ashish")))
                  ("year" . "2017")
                  ("doi" . "10.1/example")))))

    (it "searches and returns metadata for a document"
      (let ((document (papis-test--document
                       '((title . "Attention is all you need")
                         (author . "Vaswani, Ashish"))
                       "papers"))
            (result (make-hash-table :test #'equal)))
        (puthash "title" "Attention is all you need" result)
        (puthash "author" "Vaswani, Ashish" result)
        (puthash "year" "2017" result)
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) "attention"))
                  ((symbol-function 'papis--paper-search-documents)
                   (lambda (&rest _) (list result)))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt collection &rest _) (caar collection))))
          (expect (papis-paper-metadata-for-document document "crossref")
                  :to-equal
                  '(("title" . "Attention is all you need")
                    ("author_list"
                     (("family" . "Vaswani") ("given" . "Ashish")))
                    ("year" . "2017"))))))

    (it "imports metadata for a DOI"
      (let ((document (papis-test--document
                       '((title . "old") (author . "old"))
                       "papers"))
            (identifier nil))
        (cl-letf (((symbol-function 'read-string)
                   (lambda (&rest _) "10.1/example"))
                  ((symbol-function 'papis--paper-import-metadata)
                   (lambda (source value)
                     (setq identifier (list source value))
                     '(("title" . "Imported")))))
          (expect (papis-paper-metadata-for-document document "doi")
                  :to-equal '(("title" . "Imported")))
          (expect identifier :to-equal '("doi" "10.1/example"))))))

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

    (it "finds the EPUB file of a document"
      (let* ((folder (make-temp-file "papis-book-" t))
             (pdf (expand-file-name "a.pdf" folder))
             (epub (expand-file-name "b.epub" folder))
             (document (papis-test--document
                        `((title . "A Book")
                          (files . ("a.pdf" "b.epub"))
                          (_papis_local_folder . ,folder)))))
        (unwind-protect
            (progn
              (write-region "" nil pdf)
              (write-region "" nil epub)
              (expect (papis-document-epub-file document) :to-equal epub))
          (delete-directory folder t))))

    (it "finds a renamed EPUB in the document folder"
      (let* ((folder (make-temp-file "papis-book-" t))
             (epub (expand-file-name "New Name.epub" folder))
             (document (papis-test--document
                        `((title . "A Book")
                          (files . ("old-name.epub"))
                          (_papis_local_folder . ,folder)))))
        (unwind-protect
            (progn
              (write-region "" nil epub)
              (expect (papis-document-epub-file document) :to-equal epub))
          (delete-directory folder t))))

    (it "builds an embed-cover command"
      (let ((script (make-temp-file "papis-metadata-" nil ".py")))
        (unwind-protect
            (progn
              (set-file-modes script #o755)
              (let ((papis-book-metadata-script script))
                (expect (papis-document-embed-cover-command
                         "book.epub" "cover.jpg")
                        :to-equal (list script "--embed-cover"
                                        "cover.jpg" "book.epub"))))
          (delete-file script))))

    (it "embeds a cover in the document EPUB"
      (let* ((folder (make-temp-file "papis-embed-" t))
             (epub (expand-file-name "book.epub" folder))
             (cover (expand-file-name "cover.jpg" folder))
             (document (papis-test--document
                        `((title . "A Book")
                          (files . (,epub ,cover))))))
        (unwind-protect
            (progn
              (write-region "" nil epub)
              (write-region "" nil cover)
              (let ((command nil))
                (cl-letf (((symbol-function 'papis--run-metadata-command)
                           (lambda (arguments)
                             (setq command arguments)
                             "epub cover updated")))
                  (expect (papis-document-embed-cover document cover)
                          :to-equal "epub cover updated")
                  (expect (nth 1 command) :to-equal "--embed-cover")
                  (expect (nth 3 command) :to-equal epub))))
          (delete-directory folder t))))

    (it "refuses to embed a cover without an EPUB"
      (let ((document (papis-test--document
                       '((title . "A Paper")
                         (files . ("/tmp/paper.pdf"))))))
        (expect (papis-document-embed-cover document "/tmp/cover.jpg")
                :to-throw 'user-error)))

    (it "resolves relative book files against the document folder"
      (let ((document (papis-test--document
                       '((title . "A Book")
                         (files . ("book.epub"))
                         (_papis_local_folder . "/tmp/books/a-book")))))
        (expect (papis-document-book-files document)
                :to-equal '("/tmp/books/a-book/book.epub"))))

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
                           (list :status 0)))
                        ((symbol-function 'papis-document-reset-author)
                         (lambda (_document) t)))
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
                           (list :status 0)))
                        ((symbol-function 'papis-document-reset-author)
                         (lambda (_document) t)))
                (expect (papis-document-update-book-metadata document)
                        :to-equal "renamed: book.pdf -> a-book.pdf")
                (expect ran :to-equal file)
                (expect refreshed :to-be document)))
          (delete-directory folder t))))

    (it "runs the metadata script for a chosen document"
      (let ((document (papis-test--document '((title . "A Book")) "books"))
            (ran nil))
        (cl-letf (((symbol-function 'papis-read-document)
                   (lambda (&rest _) document))
                  ((symbol-function 'papis-document-update-book-metadata)
                   (lambda (doc) (setq ran doc) "renamed")))
          (expect (papis-update-book-metadata document) :to-equal "renamed")
          (expect ran :to-be document))))

    (it "prompts for a library and document"
      (let* ((document (papis-test--document
                        '((title . "A Book") (author . "A Writer"))
                        "books"))
             (prompts nil))
        (cl-letf (((symbol-function 'papis-libraries)
                   (lambda () '("books" "papers")))
                  ((symbol-function 'papis-documents)
                   (lambda (&rest _) (list document)))
                  ((symbol-function 'completing-read)
                   (lambda (prompt collection &rest _)
                     (push prompt prompts)
                     (if (equal prompt "Library: ")
                         "books"
                       (caar collection)))))
          (expect (papis-read-document) :to-be document)
          (expect (nreverse prompts)
                  :to-equal '("Library: " "Document: "))))))

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
        (cl-letf (((symbol-function 'papis-document--existing-book-files)
                   (lambda (document)
                     (papis-document-book-files document)))
                  ((symbol-function 'papis-document-update-book-metadata)
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
               '(papis-error "papis is down"))
              :to-equal "papis is down"))

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
