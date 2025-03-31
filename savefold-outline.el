;;; savefold-outline.el --- savefold for outline mode -*- lexical-binding: t; -*-

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

;; This package implements persistence for outline-mode and outline-minor-mode
;; folds.

;;; Code:

(require 'outline)
(require 'savefold-utils)

(defvar savefold-outline--folds-attr 'savefold-outline-folds)

(defun savefold-outline--recover-folds ()
  "Read and apply saved outline fold data for the current buffer."
  ;; Maybe find away to abstract out this recency check
  (if (not (savefold-utils--file-recently-modifiedp))
      (mapc
       (lambda (fold-data)
         (outline-flag-region (car fold-data) (cadr fold-data) t))
       (savefold-utils--get-file-attr savefold-outline--folds-attr))
    (message
     "savefold: Buffer contents newer than fold data for buffer '%s'. Not applying."
     (current-buffer))))

(defun savefold-outline--outline-foldp (ov)
  "Check whether OV is an outline-mode/outline-minor-mode fold overlay."
  (eq (overlay-get ov 'invisible) 'outline))

(defun savefold-outline--save-folds ()
  "Save outline fold data for the current buffer.

This also saves the modification time of the file."
  ;; Assume this means the buffer reflects the actual file state
  (when (not (buffer-modified-p))
    (savefold-utils--set-file-attr
     savefold-outline--folds-attr
     (mapcar
      (lambda (ov) `(,(overlay-start ov) ,(overlay-end ov)))
      (savefold-utils--get-overlays 'savefold-outline--outline-foldp)))
    (savefold-utils--set-file-attr-modtime)
    (savefold-utils--write-out-file-attrs)))

(defun savefold-outline--set-up-save-on-kill-buffer ()
  (add-hook 'kill-buffer-hook 'savefold-outline--save-folds nil t))

(defun savefold-outline--outline-bufferp ()
  (or (derived-mode-p 'outline-mode)
      (bound-and-true-p outline-minor-mode)))

(defun savefold-outline--save-all-buffers-folds ()
  "Save outline fold data for all buffers."
  (savefold-utils--mapc-buffers 'savefold-outline--outline-bufferp
                                'savefold-outline--save-folds))

(defun savefold-outline--set-up-save-on-kill-for-existing-buffers ()
  "Set up save on kill across all existing outline buffers."
  (savefold-utils--mapc-buffers 'savefold-outline--outline-bufferp
                                'savefold-outline--set-up-save-on-kill-buffer))

(defun savefold-outline--unhook-save-on-kill-buffer ()
  (remove-hook 'kill-buffer-hook 'savefold-outline--save-folds t))

(defun savefold-outline--unhook-save-on-kill-for-existing-buffers ()
  "Remove the save on kill hook across all existing outline buffers."
  (savefold-utils--mapc-buffers 'savefold-outline--outline-bufferp
                                'savefold-outline--unhook-save-on-kill-buffer))

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
        (add-hook 'outline-mode-hook 'savefold-outline--set-up-save-on-kill-buffer)
        (add-hook 'outline-minor-mode-hook 'savefold-outline--set-up-save-on-kill-buffer)
        (add-hook 'kill-emacs-hook 'savefold-outline--save-all-buffers-folds)

        ;; Set up save folds on existing buffers
        (savefold-outline--set-up-save-on-kill-for-existing-buffers))

    (remove-hook 'outline-mode-hook 'savefold-outline--recover-folds)
    (remove-hook 'outline-minor-mode-hook 'savefold-outline--recover-folds)

    (remove-hook 'outline-mode-hook 'savefold-outline--set-up-save-on-kill-buffer)
    (remove-hook 'outline-minor-mode-hook 'savefold-outline--set-up-save-on-kill-buffer)
    (remove-hook 'kill-emacs-hook 'savefold-outline--save-all-buffers-folds)

    (savefold-outline--unhook-save-on-kill-for-existing-buffers)))

(provide 'savefold-outline)

;;; savefold-outline.el ends here
