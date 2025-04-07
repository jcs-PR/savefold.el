;;; savefold-ts-fold.el --- savefold for ts-fold mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jen-Chieh Shen

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

;; This library implements persistence for ts-fold.el folds.

;;; Code:

(require 'font-lock)

(require 'savefold-utils)

;;
;;; Externals

(declare-function ts-fold-close "ext:ts-fold.el" (&optional node))  ; Provided by ts-fold

(declare-function tree-sitter--do-parse "ext:tree-sitter.el")

;;
;;; Implementation

(defvar savefold-ts-fold--folds-attr 'savefold-ts-fold-folds)

(defun savefold-ts-fold--recover-folds ()
  "Read and apply saved ts-fold fold data for the current buffer."
  (tree-sitter--do-parse)
  (font-lock-ensure)
  (savefold-utils--unless-file-recently-modified
   (mapc
    ;; What we've saved is really the node start+offset. Should be ok.
    (lambda (start)
      (save-excursion
        (goto-char start)
        (ts-fold-close)))
    (savefold-utils--get-file-attr savefold-ts-fold--folds-attr))))

(defun savefold-ts-fold--ts-fold-foldp (ov)
  "Check whether OV is an ts-fold fold overlay."
  (and (eq (overlay-get ov 'creator) 'ts-fold)
       (overlay-get ov 'invisible)))

(defun savefold-ts-fold--save-folds ()
  "Save ts-fold fold data for the current buffer."
  (unless (buffer-modified-p)
    (savefold-utils--set-file-attr
     savefold-ts-fold--folds-attr
     (mapcar
      #'overlay-start
      (savefold-utils--get-overlays #'savefold-ts-fold--ts-fold-foldp)))
    (savefold-utils--set-file-attr-modtime)
    (savefold-utils--write-out-file-attrs)))

(defun savefold-ts-fold--bufferp ()
  (bound-and-true-p ts-fold-mode))

;;;###autoload
(define-minor-mode savefold-ts-fold-mode
  "Toggle global persistence for ts-fold-mode folds."
  :global t
  :init-value nil
  :group 'savefold
  (if savefold-ts-fold-mode
      (progn
        (savefold-utils--require 'ts-fold)
        (savefold-utils--set-up-standard-hooks 'ts-fold
                                               '(ts-fold-mode)
                                               'savefold-ts-fold--recover-folds
                                               'savefold-ts-fold--save-folds
                                               'savefold-ts-fold--bufferp))
    (savefold-utils--unhook-standard-hooks 'ts-fold
                                           '(ts-fold-mode)
                                           'savefold-ts-fold--recover-folds
                                           'savefold-ts-fold--bufferp)))

(provide 'savefold-ts-fold)
;;; savefold-ts-fold.el ends here

