;;; savefold-treesit-fold.el --- savefold for treesit-fold mode -*- lexical-binding: t; -*-

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

;; This library implements persistence for treesit-fold folds.

;;; Code:

(require 'savefold-utils)

(declare-function treesit-fold-close "ext:treesit-fold.el" (&optional node))  ; Provided by treesit-fold

(defvar savefold-treesit-fold--folds-attr 'savefold-treesit-fold-folds)

(defun savefold-treesit-fold--recover-folds ()
  "Read and apply saved treesit-fold fold data for the current buffer."
  (savefold-utils--unless-file-recently-modified
   (mapc
    ;; What we've saved is really the node overlay start. Should be ok.
    (lambda (start)
      (save-excursion
        (goto-char start)
        (treesit-fold-close)))
    (savefold-utils--get-file-attr savefold-treesit-fold--folds-attr))))

(defun savefold-treesit-fold--treesit-fold-foldp (ov)
  "Check whether OV is an treesit-fold fold overlay."
  (and (eq (overlay-get ov 'creator) 'treesit-fold)
       (overlay-get ov 'invisible)))

(defun savefold-treesit-fold--save-folds ()
  "Save treesit-fold fold data for the current buffer."
  (unless (buffer-modified-p)
    (savefold-utils--set-file-attr
     savefold-treesit-fold--folds-attr
     (mapcar
      #'overlay-start
      (savefold-utils--get-overlays #'savefold-treesit-fold--treesit-fold-foldp)))
    (savefold-utils--set-file-attr-modtime)
    (savefold-utils--write-out-file-attrs)))

(defun savefold-treesit-fold--bufferp ()
  (bound-and-true-p treesit-fold-mode))

;;;###autoload
(define-minor-mode savefold-treesit-fold-mode
  "Toggle global persistence for treesit-fold-mode folds."
  :global t
  :init-value nil
  :group 'savefold
  (if savefold-treesit-fold-mode
      (progn
        (savefold-utils--require 'treesit-fold)
        (savefold-utils--set-up-standard-hooks 'treesit-fold
                                               '(treesit-fold-mode)
                                               'savefold-treesit-fold--recover-folds
                                               'savefold-treesit-fold--save-folds
                                               'savefold-treesit-fold--bufferp))
    (savefold-utils--unhook-standard-hooks 'treesit-fold
                                           '(treesit-fold-mode)
                                           'savefold-treesit-fold--recover-folds
                                           'savefold-treesit-fold--bufferp)))

(provide 'savefold-treesit-fold)

;;; savefold-treesit-fold.el ends here

