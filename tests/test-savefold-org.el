;;; test-savefold-org.el --- savefold-org.el tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jacob Fong

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the “Software”), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; savefold-org.el tests

;;; Code:

(require 'savefold-org)
(require 'savefold-test-utils)

(describe "savefold-org-mode"
  :var* ((source-fpath
          (expand-file-name "fixtures/org/lotr.org" savefold-test-utils--dir))
         (attr-fpath
          (expand-file-name "fixtures/org/attr-table" savefold-test-utils--dir)))

  (describe "when on"
    (before-all
      (savefold-org-mode 1))

    (it "does not recover folds upon file open if file was recently modified"
      (savefold-test-utils--with-temp-savefold-environment source-fpath attr-fpath
        (savefold-utils-set-file-attr 'savefold-modtime '(0 0 0 0) temp-source-fpath)
        (savefold-utils-write-out-file-attrs temp-source-fpath)

        (savefold-test-utils--with-open-file temp-source-fpath
          (expect
           (not (savefold-utils--get-overlays 'savefold-org--org-foldp))))))

    (it "recovers folds upon file open"
      (savefold-test-utils--with-temp-savefold-environment source-fpath attr-fpath
        (savefold-test-utils--with-open-file temp-source-fpath
          ;; Outline folds
          (expect
           (savefold-test-utils--sets-equalp
            (mapcar
             (lambda (ov) `(,(overlay-start ov) ,(overlay-end ov)))
             (savefold-utils--get-overlays 'savefold-outline--outline-foldp))
            (savefold-utils-get-file-attr savefold-outline--folds-attr)))

          ;; Strictly org folds
          (expect
           (savefold-test-utils--sets-equalp
            (mapcar
             (lambda (ov)
               `(,(overlay-start ov) ,(overlay-end ov) ,(overlay-get ov 'invisible)))
             (savefold-utils--get-overlays 'savefold-org--org-foldp))
            (savefold-utils-get-file-attr savefold-org--folds-attr))))))

    (it "saves folds upon file close"
      (savefold-test-utils--with-temp-savefold-environment source-fpath attr-fpath
        (savefold-test-utils--with-open-file temp-source-fpath
          (goto-char 548)
          (org-cycle)
          (goto-char 819)
          (outline-show-subtree))

        ;; Outline folds
        (expect
         (savefold-test-utils--sets-equalp
          (savefold-utils-get-file-attr savefold-outline--folds-attr temp-source-fpath)
          '((111 414))))

        ;; Strictly org folds
        (expect
         (savefold-test-utils--sets-equalp
          (savefold-utils-get-file-attr savefold-org--folds-attr temp-source-fpath)
          '((757 817 org-babel-hide-result) (558 616 org-babel-hide-result)))))))

  (describe "when turned back off"
    (before-all
      (savefold-org-mode -1))

    (it "does not recover folds upon file open"
      (savefold-test-utils--with-temp-savefold-environment source-fpath attr-fpath
        (savefold-test-utils--with-open-file temp-source-fpath
          (expect
           (not (savefold-utils--get-overlays 'savefold-org--org-foldp))))))

    (it "does not save folds upon file close"
      (savefold-test-utils--with-temp-savefold-environment source-fpath attr-fpath
        (savefold-test-utils--with-open-file temp-source-fpath
          (ignore))

        ;; Outline folds
        (expect
         (savefold-test-utils--sets-equalp
          (savefold-utils-get-file-attr savefold-outline--folds-attr temp-source-fpath)
          '((111 414) (846 2959))))

        ;; Strictly org folds
        (expect
         (savefold-test-utils--sets-equalp
          (savefold-utils-get-file-attr savefold-org--folds-attr temp-source-fpath)
          '((757 817 org-babel-hide-result))))))))

;;; test-savefold-org.el ends here
