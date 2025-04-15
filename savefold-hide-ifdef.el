;;; savefold-hide-ifdef.el --- savefold for hide-ifdef-mode -*- lexical-binding: t; -*-

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

;; This package implements persistence for hide-ifdef-mode folds.

;;; Code:

(require 'hideif)
(require 'savefold-utils)

(defvar savefold-hide-ifdef--folds-attr 'savefold-hide-ifdef-folds)

(defun savefold-hide-ifdef--recover-folds ()
  "Read and apply saved hide-ifdef fold data for the current buffer."
  ;; Maybe find away to abstract out this recency check
  (savefold-utils--unless-file-recently-modified
   (mapc
    (lambda (fold-data)
      (cl-destructuring-bind (start end) fold-data
        ;; hide-ifdef-region typically takes hif-find-ifdef-block points; we're
        ;; providing the corresponding overlay start and end points, which
        ;; should be fine.
        (hide-ifdef-region start end)
        (setq hide-ifdef-hiding t)
        (setq buffer-read-only
              (or hide-ifdef-read-only hif-outside-read-only))))
    (savefold-utils--get-file-attr savefold-hide-ifdef--folds-attr))))

(defun savefold-hide-ifdef--hide-ifdef-foldp (ov)
  "Check whether OV is an hide-ifdef-mode fold overlay."
  ;; Be sure to check for 'invisible so as to not catch anything when
  ;; hide-ifdef-shadow is used, but only catch true folds.
  (and (overlay-get ov 'hide-ifdef) (overlay-get ov 'invisible)))

(defun savefold-hide-ifdef--save-folds ()
  "Save hide-ifdef fold data for the current buffer."
  (unless (buffer-modified-p)
    (savefold-utils--set-file-attr
     savefold-hide-ifdef--folds-attr
     (mapcar
      (lambda (ov) `(,(overlay-start ov) ,(overlay-end ov)))
      (savefold-utils--get-overlays #'savefold-hide-ifdef--hide-ifdef-foldp)))
    (savefold-utils--set-file-attr-modtime)
    (savefold-utils--write-out-file-attrs)))

(defun savefold-hide-ifdef--bufferp ()
  (bound-and-true-p hide-ifdef-mode))

;;;###autoload
(define-minor-mode savefold-hide-ifdef-mode
  "Toggle global persistence for hide-ifdef-mode/hide-ifdef-minor-mode folds."
  :global t
  :init-value nil
  :group 'savefold
  (if savefold-hide-ifdef-mode
      (savefold-utils--set-up-standard-hooks 'hide-ifdef
                                             '(hide-ifdef-mode)
                                             'savefold-hide-ifdef--recover-folds
                                             'savefold-hide-ifdef--save-folds
                                             'savefold-hide-ifdef--bufferp)
    (savefold-utils--unhook-standard-hooks 'hide-ifdef
                                           '(hide-ifdef-mode)
                                           'savefold-hide-ifdef--recover-folds
                                           'savefold-hide-ifdef--bufferp)))

(provide 'savefold-hide-ifdef)

;;; savefold-hide-ifdef.el ends here
