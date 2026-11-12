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

    (describe "generated covers"
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
