;;; savefold-origami.el --- savefold for origami mode -*- lexical-binding: t; -*-

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

;; This library implements persistence for origami.el folds.

;;; Code:

(require 'origami)
(require 'savefold-utils)

(defvar savefold-origami--folds-attr 'savefold-origami-folds)

(defun savefold-origami--recover-folds ()
  "Read and apply saved origami fold data for the current buffer."
  (if (savefold-utils-file-recently-modifiedp)
      (message
       "savefold: Buffer contents newer than fold data for buffer '%s'. Not applying."
       (current-buffer))
    (mapc
     ;; What we've saved is really the node start+offset. Should be ok.
     (lambda (start)
       (origami-close-node (current-buffer) start))
     (savefold-utils-get-file-attr savefold-origami--folds-attr))))

(defun savefold-origami--origami-foldp (ov)
  "Check whether OV is an origami fold overlay."
  (eq (overlay-get ov 'creator) 'origami))

(defun savefold-origami--save-folds ()
  "Save origami fold data for the current buffer."
  (when (not (buffer-modified-p))
    (savefold-utils-set-file-attr
     savefold-origami--folds-attr
     (mapcar
      'overlay-start
      (seq-filter
       (lambda (ov)
         (and (savefold-origami--origami-foldp ov)
              (overlay-get ov 'invisible)))
       (overlays-in (point-min) (point-max)))))
    (savefold-utils-set-file-attr-modtime)
    (savefold-utils-write-out-file-attrs)))

(defun savefold-origami--set-up-save-on-kill-buffer ()
  (add-hook 'kill-buffer-hook 'savefold-origami--save-folds nil t))

(defun savefold-origami--origami-bufferp ()
  (bound-and-true-p origami-mode))

(defun savefold-origami--save-all-buffers-folds ()
  "Save origami fold data for all buffers."
  (savefold-utils--mapc-buffers 'savefold-origami--origami-bufferp
                                'savefold-origami--save-folds))

(defun savefold-origami--set-up-save-on-kill-for-existing-buffers ()
  "Set up save on kill across all existing origami buffers."
  (savefold-utils--mapc-buffers 'savefold-origami--origami-bufferp
                                'savefold-origami--set-up-save-on-kill-buffer))

(defun savefold-origami--unhook-save-on-kill-buffer ()
  (remove-hook 'kill-buffer-hook 'savefold-origami--save-folds t))

(defun savefold-origami--unhook-save-on-kill-for-existing-buffers ()
  (savefold-utils--mapc-buffers 'savefold-origami--origami-bufferp
                                'savefold-origami--unhook-save-on-kill-buffer))

(define-minor-mode savefold-origami-mode
  "Toggle global persistence for origami-mode folds."
  :global t
  :init-value nil
  (if savefold-origami-mode
      (progn
        ;; Recover folds upon file open
        (add-hook 'origami-mode-hook 'savefold-origami--recover-folds)

        ;; Save folds on file close
        (add-hook 'origami-mode-hook 'savefold-origami--set-up-save-on-kill-buffer)
        (add-hook 'kill-emacs-hook 'savefold-origami--save-all-buffers-folds)

        ;; Set up save folds on existing buffers
        (savefold-origami--set-up-save-on-kill-for-existing-buffers))

    (remove-hook 'origami-mode-hook 'savefold-origami--recover-folds)

    (remove-hook 'origami-mode-hook 'savefold-origami--set-up-save-on-kill-buffer)
    (remove-hook 'kill-emacs-hook 'savefold-origami--save-all-buffers-folds)

    (savefold-origami--unhook-save-on-kill-for-existing-buffers)))

(provide 'savefold-origami)

;;; savefold-origami.el ends here

