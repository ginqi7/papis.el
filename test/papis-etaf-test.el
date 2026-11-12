;;; papis-etaf-test.el --- Tests for the ETAF Papis gallery  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; These tests cover the rendering and cover helpers of `papis-etaf.el'
;; that do not need a graphical frame or a running Papis executable.  The
;; whole file is skipped when ETAF is not installed.

;;; Code:

(require 'buttercup)

(when (require 'etaf nil t)
  (require 'papis-etaf)

  (defun papis-etaf-test--document (data)
    "Create a Papis document from DATA."
    (car (papis--parse-documents (list data) "books")))

  (describe "papis-etaf"
    (describe "EPUB paths"
      (it "resolves manifest hrefs without leaving the archive"
        (expect (papis-etaf--epub-resolve "OEBPS/" "images/cover.jpg")
                :to-equal "OEBPS/images/cover.jpg")
        (expect (papis-etaf--epub-resolve "OEBPS/" "../images/cover.jpg")
                :to-equal "images/cover.jpg")
        (expect (papis-etaf--epub-resolve "" "OEBPS/cover.jpg")
                :to-equal "OEBPS/cover.jpg"))

      (it "decodes URL escapes and backslashes"
        (expect (papis-etaf--epub-resolve "OEBPS/" "a%20b\\c.jpg")
                :to-equal "OEBPS/a b/c.jpg")))

    (describe "text helpers"
      (it "wraps words into lines of the requested width"
        (expect (papis-etaf--wrap-words "one two three four" 9)
                :to-equal '("one two" "three" "four")))

      (it "breaks a word that is wider than one line"
        (expect (papis-etaf--wrap-words "abcdefghijkl" 5)
                :to-equal '("abcde" "fghij" "kl")))

      (it "truncates with an ellipsis"
        ;; U+2026 is one column in batch but two with some CJK fonts, so
        ;; assert the width and the ellipsis instead of literal text.
        (let ((truncated (papis-etaf--truncate "abcdef" 4)))
          (expect (string-width truncated) :to-equal 4)
          (expect (string-suffix-p "…" truncated) :to-be-truthy))
        (expect (papis-etaf--truncate "abc" 4) :to-equal "abc"))

      (it "shows the attached book file name"
        (let ((document (papis-etaf-test--document
                         '((title . "A Book")
                           (files . ("/tmp/A Book (2020).epub"))))))
          (expect (papis-etaf--document-file-name document)
                  :to-equal "A Book (2020).epub"))))

    (describe "tags"
      (it "offers preset tags and tags already in the library"
        (let ((first (papis-etaf-test--document
                      '((title . "A") (tags . ("lisp")))))
              (second (papis-etaf-test--document
                       '((title . "B") (tags . ("emacs" "reading"))))))
          (expect (papis-etaf--tag-candidates (list first second))
                  :to-have-same-items-as
                  '("reading" "read" "lisp" "emacs")))))

    (describe "generated covers"
      (it "builds typographic covers for papers"
        (let* ((document
                (papis-etaf-test--document
                 '((title . "MapReduce: Simplified Data Processing")
                   (author . "Dean, Jeffrey and Ghemawat, Sanjay")
                   (year . 2004)
                   (type . "inproceedings")
                   (booktitle . "OSDI")
                   (doi . "10.5555/1251254"))))
               (svg (papis-etaf--paper-cover-svg document)))
          (expect (papis-etaf--paper-p document) :to-be-truthy)
          (expect (string-match-p "PAPER" svg) :to-be-truthy)
          (expect (string-match-p "MapReduce" svg) :to-be-truthy)
          (expect (string-match-p "OSDI" svg) :to-be-truthy)
          (expect (string-match-p "10.5555/1251254" svg) :to-be-truthy)))

      (it "uses the most recently attached cover"
        (let* ((folder (make-temp-file "papis-cover-" t))
               (jpg (expand-file-name "cover.jpg" folder))
               (svg (expand-file-name "cover.svg" folder))
               (first (papis-etaf-test--document
                       (list (cons 'title "A Book")
                             (cons 'files (list "cover.jpg" "cover.svg"))
                             (cons '_papis_local_folder folder))))
               (second (papis-etaf-test--document
                        (list (cons 'title "A Book")
                              (cons 'files (list "cover.svg" "cover.jpg"))
                              (cons '_papis_local_folder folder)))))
          (unwind-protect
              (progn
                (write-region "" nil jpg)
                (write-region "" nil svg)
                (expect (papis-etaf--cover-source first) :to-equal svg)
                (expect (papis-etaf--cover-source second) :to-equal jpg))
            (delete-directory folder t))))

      (it "prefers the document folder for cover files"
        (let* ((folder (make-temp-file "papis-cover-" t))
               (document (papis-etaf-test--document
                          (list (cons 'title "A Book")
                                (cons '_papis_local_folder folder)))))
          (unwind-protect
              (expect (papis-etaf--cover-destination document "jpg" "key")
                      :to-equal (expand-file-name "cover.jpg" folder))
            (delete-directory folder t))))

      (it "saves generated covers beside the document and attaches them"
        (let* ((folder (make-temp-file "papis-cover-" t))
               (document (papis-etaf-test--document
                          `((title . "A Book")
                            (_papis_local_folder . ,folder))))
               (attached nil))
          (unwind-protect
              (progn
                (cl-letf (((symbol-function 'papis-document-attach-file)
                           (lambda (_document file)
                             (setq attached file))))
                  (let ((cover
                         (papis-etaf--write-generated-cover
                          document
                          (papis-etaf--paper-cover-svg document))))
                    (expect cover
                            :to-equal (expand-file-name "cover.svg" folder))
                    (expect (file-exists-p cover) :to-be-truthy)
                    (expect (papis-etaf--generated-cover-p cover)
                            :to-be-truthy)
                    (expect attached :to-equal "cover.svg"))))
            (delete-directory folder t))))

      (it "does not classify books as papers"
        (let ((document
               (papis-etaf-test--document
                '((title . "A Book") (type . "book")))))
          (expect (papis-etaf--paper-p document) :to-be nil)))
      (it "escapes metadata in the SVG"
        (let* ((document
                (papis-etaf-test--document
                 '((title . "A & B <C>")
                   (author . "D 'E'")
                   (files . ("/tmp/book.epub")))))
               (svg (papis-etaf--placeholder-svg document)))
          (expect (string-match-p "A &amp; B &lt;C&gt;" svg) :to-be-truthy)
          (expect (string-match-p "D &apos;E&apos;" svg) :to-be-truthy)
          (expect (string-match-p "A & B" svg) :to-be nil)))

      (it "puts wrapped title lines on separate baselines"
        (let* ((document
                (papis-etaf-test--document
                 '((title . "One Two Three Four Five Six Seven Eight Nine Ten")
                   (author . "Author")
                   (files . ("/tmp/book.epub")))))
               (svg (papis-etaf--placeholder-svg document)))
          (expect (string-match-p "y=\"332\"" svg) :to-be-truthy)
          (expect (string-match-p "y=\"404\"" svg) :to-be-truthy))))))

;;; papis-etaf-test.el ends here
