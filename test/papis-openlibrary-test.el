;;; papis-openlibrary-test.el --- Tests for Open Library metadata  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Qiqi Jin

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; These tests cover the Open Library result parsing and the
;; interactive metadata lookup.  The network request is stubbed, so
;; they do not need openlibrary.el or an internet connection.

;;; Code:

(require 'buttercup)
(require 'papis-openlibrary)

(defun papis-openlibrary-test--document (data)
  "Create a Papis document from DATA."
  (car (papis--parse-documents (list data) "books")))

(defconst papis-openlibrary-test--result
  '(("title" . "The Little Schemer")
    ("author_name" . ("Daniel P. Friedman" "Matthias Felleisen"))
    ("first_publish_year" . 1995)
    ("publisher" . ("MIT Press"))
    ("isbn" . ("0262560992" "9780262560993")))
  "Open Library search result used by the tests.")

(describe "papis-openlibrary"
  (describe "search results"
    (it "labels results with title, authors and year"
      (expect (papis-openlibrary-label papis-openlibrary-test--result)
              :to-equal
              "The Little Schemer — Daniel P. Friedman, Matthias Felleisen (1995)"))

    (it "splits author names into family and given parts"
      (expect (papis-openlibrary--author-entry "Kleppmann, Martin")
              :to-equal '(("family" . "Kleppmann") ("given" . "Martin")))
      (expect (papis-openlibrary--author-entry "Martin Kleppmann")
              :to-equal '(("family" . "Kleppmann") ("given" . "Martin"))))

    (it "builds Papis fields and prefers a 13-digit ISBN"
      (expect (papis-openlibrary-metadata papis-openlibrary-test--result)
              :to-equal
              '(("title" . "The Little Schemer")
                ("author_list" .
                 ((("family" . "Friedman") ("given" . "Daniel P."))
                  (("family" . "Felleisen") ("given" . "Matthias"))))
                ("year" . "1995")
                ("publisher" . "MIT Press")
                ("isbn" . "9780262560993")))))

  (describe "document lookup"
    (it "builds a title and author query"
      (let ((document (papis-openlibrary-test--document
                       '((title . "The Little Schemer")
                         (author . "Friedman")))))
        (expect (papis-openlibrary-query document)
                :to-equal "The Little Schemer Friedman")))

    (it "returns the chosen fields"
      (let ((document (papis-openlibrary-test--document
                       '((papis_id . "abc123")
                         (title . "Old Title")))))
        (cl-letf (((symbol-function 'papis-openlibrary-search)
                   (lambda (_query &optional _isbn)
                     (list papis-openlibrary-test--result)))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _)
                     (papis-openlibrary-label
                      papis-openlibrary-test--result)))
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (expect (papis-openlibrary-metadata-for-document document)
                  :to-equal
                  '(("title" . "The Little Schemer")
                    ("author_list" .
                     ((("family" . "Friedman") ("given" . "Daniel P."))
                      (("family" . "Felleisen") ("given" . "Matthias"))))
                    ("year" . "1995")
                    ("publisher" . "MIT Press")
                    ("isbn" . "9780262560993"))))))

    (it "returns nil when the update is declined"
      (let ((document (papis-openlibrary-test--document
                       '((papis_id . "abc123")
                         (title . "Old Title")))))
        (cl-letf (((symbol-function 'papis-openlibrary-search)
                   (lambda (_query &optional _isbn)
                     (list papis-openlibrary-test--result)))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _)
                     (papis-openlibrary-label
                      papis-openlibrary-test--result)))
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
          (expect (papis-openlibrary-metadata-for-document document)
                  :to-be nil)))))

  (describe "covers"
    (it "prefers a cover id from the result"
      (expect (papis-openlibrary-cover-spec
               '(("cover_i" . 123)
                 ("cover_edition_key" . "OL7353617M")))
              :to-equal '(123 . id)))

    (it "falls back to the document ISBN"
      (let ((document (papis-openlibrary-test--document
                       '((title . "The Little Schemer")
                         (isbn . ("0262560992" "9780262560993"))))))
        (expect (papis-openlibrary-cover-spec
                 '(("key" . "/works/OL478363W")) document)
                :to-equal '("0262560992" . isbn))))

    (it "falls back to the cover edition"
      (expect (papis-openlibrary-cover-spec
               '(("cover_edition_key" . "OL7353617M")))
              :to-equal '("OL7353617M" . olid)))

    (it "returns nil without a cover candidate"
      (expect (papis-openlibrary-cover-spec '(("title" . "A Book")))
              :to-be nil))))

;;; papis-openlibrary-test.el ends here
