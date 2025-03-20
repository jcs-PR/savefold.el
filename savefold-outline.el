;;; savefold-outline.el --- savefold for outline mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jacob Fong
;; Author: Jacob Fong <jacobcfong@gmail.com>
;; Version: 0.1
;; Homepage:

;; Package-Requires: ((emacs "28.2"))

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

;; This package implements persistence for outline-mode and outline-minor-mode
;; folds.

;;; Code:

(require 'outline)
(require 'savefold-utils)

(defvar-local savefold-outline--folds '()
  "A list containing data for each outline fold in the current buffer.

It's a list of pairs (start . end) for the start and end points of the
corresponding fold overlay.")

 ;; check mod time
(defun savefold-outline--recover-folds ()
  "Read and apply saved outline fold data for the current buffer."
  (mapc
   (lambda (fold-data)
     (outline-flag-region (car fold-data) (cdr fold-data) t))
   (savefold-utils-get-file-attr 'savefold-outline--folds)))

(defun savefold-outline--setup-save-on-kill-buffer ()
  (add-hook 'kill-buffer-hook 'savefold-outline--save-folds nil t))

(defun savefold-outline--outline-foldp (ov)
  "Checks whether OV is an outline-mode/outline-minor-mode fold overlay."
  (eq (overlay-get ov 'invisible) 'outline))

(defun savefold-outline--save-folds ()
  "Save outline fold data for the current buffer."
  (setq savefold-outline--folds '())
  (mapc
   (lambda (ov)
     (when (savefold-outline--outline-foldp ov)
       (add-to-list 'savefold-outline--folds
                    `(,(overlay-start ov) . ,(overlay-end ov)))))
   ;; overlays-in does not necessarily return overlays in order
   (overlays-in (point-min) (point-max)))
  (savefold-utils-set-file-attr 'savefold-outline--folds
                                savefold-outline--folds)
  (savefold-utils-write-out-file-attrs))

(defun savefold-outline--save-all-buffers-folds ()
  "Save fold data across all outline-mode/outline-minor-mode buffers."
  (mapc
   (lambda (buf)
     (with-current-buffer buf
       (when (or (derived-mode-p 'outline-mode)
                 (bound-and-true-p outline-minor-mode))
         (savefold-outline--save-folds))))
   (buffer-list)))

;;;###autoload
(define-minor-mode savefold-outline-mode
  "Toggle global persistence for outline-mode/outline-minor-mode folds."
  :global t
  :init-value nil
  (if savefold-outline-mode
      (progn
        ;; Recover folds upon file open
        (add-hook 'outline-mode-hook 'savefold-outline--recover-folds)
        (add-hook 'outline-minor-mode-hook 'savefold-outline--recover-folds)

        ;; Save folds on file close
        (add-hook 'outline-mode-hook 'savefold-outline--setup-save-on-kill-buffer)
        (add-hook 'outline-minor-mode-hook 'savefold-outline--setup-save-on-kill-buffer)

        ;; Save existing folds
        (savefold-outline--save-all-buffers-folds))
    (remove-hook 'outline-minor-mode 'savefold-outline--recover-folds)
    (remove-hook 'outline-minor-mode-hook 'savefold-outline--recover-folds)
    (remove-hook 'outline-mode-hook 'savefold-outline--setup-save-on-kill-buffer)
    (remove-hook 'outline-minor-mode-hook 'savefold-outline--setup-save-on-kill-buffer)))

(provide 'savefold-outline)

;;; savefold-outline.el ends here
