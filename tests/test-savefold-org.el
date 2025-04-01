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

;; Use text property style folding when available. Assume org is not loaded at
;; this point.
;; Probably should also test old-fashioned folding in the newer versions
(when (version< "29" emacs-version)
    (setq org-fold-core-style 'text-properties))

(require 'savefold-org)
(require 'savefold-test-utils)

(defun test-savefold-org--old-fashioned ()
  (describe "savefold-org-mode (old-fashioned folds)"
    :var* ((source-fpath
            (expand-file-name
             "fixtures/org/test.org" savefold-test-utils--dir))
           (attr-fpath
            (expand-file-name
             "fixtures/org/old-fashioned-attr-table" savefold-test-utils--dir)))

    (describe "when on"
      (before-all
        (savefold-org-mode 1))

      (it "does not recover folds upon file open if file was recently modified"
        (savefold-test-utils--with-temp-savefold-environment source-fpath attr-fpath
          (savefold-utils--set-file-attr 'savefold-modtime '(0 0 0 0) temp-source-fpath)
          (savefold-utils--write-out-file-attrs temp-source-fpath)

          (savefold-test-utils--with-open-file temp-source-fpath
            (expect
             (and
              (not
               (savefold-utils--get-overlays 'savefold-outline--outline-foldp))
              (not
               (savefold-utils--get-overlays 'savefold-org--old-fashioned-foldp)))))))

      (it "recovers folds upon file open"
        (savefold-test-utils--with-temp-savefold-environment source-fpath attr-fpath
          (savefold-test-utils--with-open-file temp-source-fpath
            ;; Outline folds
            (expect
             (savefold-test-utils--sets-equalp
              (mapcar
               (lambda (ov) `(,(overlay-start ov) ,(overlay-end ov)))
               (savefold-utils--get-overlays 'savefold-outline--outline-foldp))
              (savefold-utils--get-file-attr savefold-outline--folds-attr)))

            ;; Strictly org folds
            (expect
             (savefold-test-utils--sets-equalp
              (mapcar
               (lambda (ov)
                 `(,(overlay-start ov) ,(overlay-end ov) ,(overlay-get ov 'invisible)))
               (savefold-utils--get-overlays 'savefold-org--old-fashioned-foldp))
              (savefold-utils--get-file-attr savefold-org--old-fashioned-folds-attr))))))

      (it "saves folds upon file close"
        (savefold-test-utils--with-temp-savefold-environment source-fpath attr-fpath
          (savefold-test-utils--with-open-file temp-source-fpath
            (goto-char 773)
            (org-hide-drawer-toggle)
            (goto-char 896)
            (org-hide-block-toggle)
            (goto-char 964)
            (org-babel-hide-result-toggle)
            (goto-char 1057)
            (org-hide-entry))

          ;; Outline folds
          (expect
           (savefold-test-utils--sets-equalp
            (savefold-utils--get-file-attr savefold-outline--folds-attr temp-source-fpath)
            '((786 819) (676 705) (367 400) (1087 1116))))

          ;; Strictly org folds
          (expect
           (savefold-test-utils--sets-equalp
            (savefold-utils--get-file-attr savefold-org--old-fashioned-folds-attr temp-source-fpath)
            '((974 1003 org-babel-hide-result)
              (913 962 org-hide-block)
              (558 587 org-babel-hide-result)
              (497 546 org-hide-block)))))))

    (describe "when turned back off"
      (before-all
        (savefold-org-mode -1))

      (it "does not recover folds upon file open"
        (savefold-test-utils--with-temp-savefold-environment source-fpath attr-fpath
          (savefold-test-utils--with-open-file temp-source-fpath
            (expect
             (and
              (not
               (savefold-utils--get-overlays 'savefold-outline--outline-foldp))
              (not
               (savefold-utils--get-overlays 'savefold-org--old-fashioned-foldp)))))))

      (it "does not save folds upon file close"
        (savefold-test-utils--with-temp-savefold-environment source-fpath attr-fpath
          (savefold-test-utils--with-open-file temp-source-fpath
            (ignore))

          ;; Outline folds
          (expect
           (savefold-test-utils--sets-equalp
            (savefold-utils--get-file-attr savefold-outline--folds-attr temp-source-fpath)
            '((676 705) (367 400))))

          ;; Strictly org folds
          (expect
           (savefold-test-utils--sets-equalp
            (savefold-utils--get-file-attr savefold-org--old-fashioned-folds-attr temp-source-fpath)
            '((558 587 org-babel-hide-result)
              (497 546 org-hide-block)))))))))

(defun test-savefold-org--current ()
  (describe "savefold-org-mode (text-property folds)"
    :var* ((source-fpath
            (expand-file-name "fixtures/org/test.org" savefold-test-utils--dir))
           (attr-fpath
            (expand-file-name "fixtures/org/current-attr-table" savefold-test-utils--dir)))

    (describe "when on"
      (before-all
        (savefold-org-mode 1))

      (it "does not recover folds upon file open if file was recently modified"
        (savefold-test-utils--with-temp-savefold-environment source-fpath attr-fpath
          (savefold-utils--set-file-attr 'savefold-modtime '(0 0 0 0) temp-source-fpath)
          (savefold-utils--write-out-file-attrs temp-source-fpath)

          (savefold-test-utils--with-open-file temp-source-fpath
            ;; Text property folds
            (expect
             (not
              (org-fold-get-regions :specs savefold-org--text-prop-fold-specs)))

            ;; Overlay folds
            (expect
             (not
              (savefold-utils--get-overlays 'savefold-org--overlay-foldp))))))

      (it "recovers folds upon file open"
        (savefold-test-utils--with-temp-savefold-environment source-fpath attr-fpath
          (savefold-test-utils--with-open-file temp-source-fpath
            ;; Text property folds
            (expect
             (savefold-test-utils--sets-equalp
              (org-fold-get-regions :specs savefold-org--text-prop-fold-specs)
              (savefold-utils--get-file-attr savefold-org--text-prop-folds-attr)))

            ;; Overlay folds
            (expect
             (savefold-test-utils--sets-equalp
              (mapcar
               (lambda (ov)
                 `(,(overlay-start ov) ,(overlay-end ov) ,(overlay-get ov 'invisible)))
               (savefold-utils--get-overlays 'savefold-org--overlay-foldp))
              (savefold-utils--get-file-attr savefold-org--overlay-folds-attr))))))

      (it "saves folds upon file close"
        (savefold-test-utils--with-temp-savefold-environment source-fpath attr-fpath
          (savefold-test-utils--with-open-file temp-source-fpath
            (goto-char 773)
            (org-fold-hide-drawer-toggle)
            (goto-char 896)
            (org-fold-hide-block-toggle)
            (goto-char 964)
            (org-babel-hide-result-toggle)
            (goto-char 1057)
            (org-fold-hide-entry))

          ;; Text property folds
          (expect
           (savefold-test-utils--sets-equalp
            (savefold-utils--get-file-attr
             savefold-org--text-prop-folds-attr temp-source-fpath)
            '((786 819 org-fold-drawer)
              (367 400 org-fold-drawer)
              (913 962 org-fold-block)
              (497 546 org-fold-block)
              (1087 1116 org-fold-outline)
              (676 705 org-fold-outline))))

          ;; Overlay folds
          (expect
           (savefold-test-utils--sets-equalp
            (savefold-utils--get-file-attr
             savefold-org--overlay-folds-attr temp-source-fpath)
            '((558 587 org-babel-hide-result)
              (974 1003 org-babel-hide-result)))))))

    (describe "when turned back off"
      (before-all
        (savefold-org-mode -1))

      (it "does not recover folds upon file open"
        (savefold-test-utils--with-temp-savefold-environment source-fpath attr-fpath
          (savefold-test-utils--with-open-file temp-source-fpath
            ;; Text property folds
            (expect
             (not
              (org-fold-get-regions :specs savefold-org--text-prop-fold-specs)))

            ;; Overlay folds
            (expect
             (not
              (savefold-utils--get-overlays 'savefold-org--overlay-foldp))))))

      (it "does not save folds upon file close"
        (savefold-test-utils--with-temp-savefold-environment source-fpath attr-fpath
          (savefold-test-utils--with-open-file temp-source-fpath
            (ignore))

          ;; Text property folds
          (expect
           (savefold-test-utils--sets-equalp
            (savefold-utils--get-file-attr
             savefold-org--text-prop-folds-attr temp-source-fpath)
            '((367 400 org-fold-drawer)
              (497 546 org-fold-block)
              (676 705 org-fold-outline))))

          ;; Overlay folds
          (expect
           (savefold-test-utils--sets-equalp
            (savefold-utils--get-file-attr
             savefold-org--overlay-folds-attr temp-source-fpath)
            '((558 587 org-babel-hide-result)))))))))

(if (savefold-org--using-old-fashioned-foldsp)
    (test-savefold-org--old-fashioned)
  (test-savefold-org--current))

;;; test-savefold-org.el ends here
