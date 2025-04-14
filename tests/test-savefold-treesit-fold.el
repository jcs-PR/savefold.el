;;; test-savefold-treesit-fold.el --- savefold-treesit-fold.el tests -*- lexical-binding: t; -*-

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

;; savefold-treesit-fold.el tests

;;; Code:

(if (version< emacs-version "29")
    (message "savefold: skip treesit-fold testing in version 28 and below")

  ;; We need to locate the Python parser from tree-sitter-langs, see
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Language-Grammar.html
  (setq treesit-extra-load-path (list
                                 (file-name-concat
                                  package-user-dir
                                  (car
                                   (seq-filter
                                    (lambda (dname)
                                      (and
                                       (length> dname 16)
                                       (equal (substring dname 0 17) "tree-sitter-langs")))
                                    (directory-files package-user-dir)))
                                  "bin")))
  (setq treesit-load-name-override-list '((python "python" "tree_sitter_python")))

  (require 'savefold-treesit-fold)
  (require 'savefold-test-utils)

  (describe "savefold-treesit-fold-mode"
    :var* ((source-fpath
            (expand-file-name "fixtures/treesit-fold/eight-queens.py" savefold-test-utils--dir))
           (attr-fpath
            (expand-file-name "fixtures/treesit-fold/attr-table" savefold-test-utils--dir)))

    (describe "when on"
      (before-all
        (savefold-treesit-fold-mode 1))

      (it "does not recover folds upon file open if file was recently modified"
        (savefold-test-utils--with-temp-savefold-environment source-fpath attr-fpath
          (savefold-utils--set-file-attr 'savefold-modtime '(0 0 0 0) temp-source-fpath)
          (savefold-utils--write-out-file-attrs temp-source-fpath)

          (savefold-test-utils--with-open-file temp-source-fpath
            (python-ts-mode)
            (treesit-fold-mode 1)
            (expect
             (not (savefold-utils--get-overlays 'savefold-treesit-fold--treesit-fold-foldp))))))

      (it "recovers folds upon file open"
        (savefold-test-utils--with-temp-savefold-environment source-fpath attr-fpath
          (savefold-test-utils--with-open-file temp-source-fpath
            (python-ts-mode)
            (treesit-fold-mode 1)
            (expect
             (savefold-test-utils--sets-equalp
              (mapcar
               'overlay-start
               (savefold-utils--get-overlays 'savefold-treesit-fold--treesit-fold-foldp))
              (savefold-utils--get-file-attr savefold-treesit-fold--folds-attr))))))

      (it "saves folds upon file close"
        (savefold-test-utils--with-temp-savefold-environment source-fpath attr-fpath
          (savefold-test-utils--with-open-file temp-source-fpath
            (python-ts-mode)
            (treesit-fold-mode 1)
            (goto-char 111)
            (treesit-fold-open)
            (goto-char 306)
            (treesit-fold-close))

          (expect
           (savefold-test-utils--sets-equalp
            (savefold-utils--get-file-attr savefold-treesit-fold--folds-attr temp-source-fpath)
            '(100 328))))))

    (describe "when turned back off"
      (before-all
        (savefold-treesit-fold-mode -1))

      (it "does not recover folds upon file open"
        (savefold-test-utils--with-temp-savefold-environment source-fpath attr-fpath
          (savefold-test-utils--with-open-file temp-source-fpath
            (python-ts-mode)
            (treesit-fold-mode 1)
            (expect
             (not (savefold-utils--get-overlays 'savefold-treesit-fold--treesit-fold-foldp))))))

      (it "does not save folds upon file close"
        (savefold-test-utils--with-temp-savefold-environment source-fpath attr-fpath
          (savefold-test-utils--with-open-file temp-source-fpath
            (python-ts-mode)
            (treesit-fold-mode 1))

          (expect
           (savefold-test-utils--sets-equalp
            (savefold-utils--get-file-attr savefold-treesit-fold--folds-attr temp-source-fpath)
            '(100 132))))))))

;;; test-savefold-origami.el ends here

