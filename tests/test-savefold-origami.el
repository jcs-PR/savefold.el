;;; test-savefold-origami.el --- savefold-origami.el tests -*- lexical-binding: t; -*-

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

;; savefold-origami.el tests

;;; Code:

(require 'savefold-origami)
(require 'savefold-test-utils)

(describe "savefold-origami-mode"
  :var* ((source-fpath
          (expand-file-name "fixtures/origami/eight-queens.py" savefold-test-utils--dir))
         (attr-fpath
          (expand-file-name "fixtures/origami/attr-table" savefold-test-utils--dir)))

  (describe "when on"
    (before-all
      (savefold-origami-mode 1))

    (it "does not recover folds upon file open if file was recently modified"
      (savefold-test-utils--with-temp-savefold-environment source-fpath attr-fpath
        (savefold-utils--set-file-attr 'savefold-modtime '(0 0 0 0) temp-source-fpath)
        (savefold-utils--write-out-file-attrs temp-source-fpath)

        (savefold-test-utils--with-open-file temp-source-fpath
          (origami-mode 1)
          (expect
           (not (savefold-utils--get-overlays 'savefold-origami--origami-foldp))))))

    (it "recovers folds upon file open"
      (savefold-test-utils--with-temp-savefold-environment source-fpath attr-fpath
        (savefold-test-utils--with-open-file temp-source-fpath
          (origami-mode 1)
          (expect
           (savefold-test-utils--sets-equalp
            (mapcar
             'overlay-start
             (savefold-utils--get-overlays 'savefold-origami--origami-foldp))
            (savefold-utils--get-file-attr savefold-origami--folds-attr))))))

    (it "saves folds upon file close"
      (savefold-test-utils--with-temp-savefold-environment source-fpath attr-fpath
        (savefold-test-utils--with-open-file temp-source-fpath
          (origami-mode 1)
          (origami-close-node (current-buffer) 111))

        (expect
         (savefold-test-utils--sets-equalp
          (savefold-utils--get-file-attr savefold-origami--folds-attr temp-source-fpath)
          '(100 132 328)))))

    ;; How to test? (it "saves folds upon killing emacs")
    )

  (describe "when turned back off"
    (before-all
      (savefold-origami-mode -1))

    (it "does not recover folds upon file open"
      (savefold-test-utils--with-temp-savefold-environment source-fpath attr-fpath
        (savefold-test-utils--with-open-file temp-source-fpath
          (origami-mode 1)
          (expect
           (not (savefold-utils--get-overlays 'savefold-origami--origami-foldp))))))

    (it "does not save folds upon file close"
      (savefold-test-utils--with-temp-savefold-environment source-fpath attr-fpath
        (savefold-test-utils--with-open-file temp-source-fpath
          (origami-mode 1))

        (expect
         (savefold-test-utils--sets-equalp
          (savefold-utils--get-file-attr savefold-origami--folds-attr temp-source-fpath)
          '(100 328)))))))

;;; test-savefold-origami.el ends here
