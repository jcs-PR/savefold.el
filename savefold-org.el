;;; savefold-org.el --- savefold for org mode -*- lexical-binding: t; -*-

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

;; This package implements persistence for org-mode visibility.

;;; Code:

(require 'org)
(require 'savefold-utils)
(require 'savefold-outline)

(defcustom savefold-org-inhibit-outline-integration nil
  "If non-nil, do not automatically run outline-mode backend functions.

By default, the outline backend is manually integrated with the org backend at
some points, due to the special relationship between org-mode and
outline-mode.

A value of nil is required for folds created with `org-flag-region' with an
'outline invisibility spec to be saved, for example. There is not really any
reason for this to be non-nil."
  :type 'boolean
  :group 'savefold)

(defvar savefold-org--org-fold-specs '(org-hide-block org-babel-hide-result))

(defvar savefold-org--folds-attr 'savefold-org-folds)

(defun savefold-org--recover-folds ()
  "Read and apply saved org fold data for the current buffer."
  (if (not (savefold-utils-file-recently-modifiedp))
      (progn
        (when (and (not savefold-org-inhibit-outline-integration)
                   (not savefold-outline-mode))
          (savefold-outline--recover-folds))
        (mapc
         (lambda (fold-data)
           (let ((start (car fold-data))
                 (end (cadr fold-data))
                 (spec (caddr fold-data)))
             (cond
              ((eq spec 'org-babel-hide-result)
               (save-excursion
                 (goto-char start)
                 (org-babel-hide-result-toggle-maybe)))
              (t (org-flag-region start end t spec)))))
         (savefold-utils-get-file-attr savefold-org--folds-attr)))
    (message
     "savefold: Buffer contents newer than fold data for buffer '%s'. Not applying."
     (current-buffer))))

(defun savefold-org--org-foldp (ov)
  "Check whether the overlay is for an org-mode fold.

This does not include folds made with `org-flag-region' with an 'outline
invisibility spec, but only the invisibility specs exclusive to org-mode:
`savefold-org--org-fold-specs'."
  (memq (overlay-get ov 'invisible) savefold-org--org-fold-specs))

(defun savefold-org--save-folds ()
  "Save org fold data for the current buffer."
  (when (not (buffer-modified-p))
    (when (and (not savefold-org-inhibit-outline-integration)
               (not savefold-outline-mode))
      (savefold-outline--save-folds))
    (savefold-utils-set-file-attr
     savefold-org--folds-attr
     (mapcar
      (lambda (ov)
        `(,(overlay-start ov) ,(overlay-end ov) ,(overlay-get ov 'invisible)))
      (savefold-utils--get-overlays 'savefold-org--org-foldp)))
    (savefold-utils-set-file-attr-modtime)
    (savefold-utils-write-out-file-attrs)))

(defun savefold-org--set-up-save-on-kill-buffer ()
  (add-hook 'kill-buffer-hook 'savefold-org--save-folds nil t))

(defun savefold-org--org-bufferp ()
  (derived-mode-p 'org-mode))

(defun savefold-org--save-all-buffers-folds ()
  "Save org fold data for all buffers."
  (savefold-utils--mapc-buffers 'savefold-org--org-bufferp
                                'savefold-org--save-folds))

(defun savefold-org--set-up-save-on-kill-for-existing-buffers ()
  "Set up save on kill across all existing org buffers."
  (savefold-utils--mapc-buffers 'savefold-org--org-bufferp
                                'savefold-org--set-up-save-on-kill-buffer))

(defun savefold-org--unhook-save-on-kill-buffer ()
  (remove-hook 'kill-buffer-hook 'savefold-org--save-folds t))

(defun savefold-org--unhook-save-on-kill-for-existing-buffers ()
  "Remove the save on kill hook across all existing org buffers."
  (savefold-utils--mapc-buffers 'savefold-org--org-bufferp
                                'savefold-org--unhook-save-on-kill-buffer))

(define-minor-mode savefold-org-mode
  "Toggle global persistence for org-mode folds."
  :global t
  :init-value nil
  (if savefold-org-mode
      (progn
        ;; Recover folds upon file open
        (add-hook 'org-mode-hook 'savefold-org--recover-folds)

        ;; Save folds on file close
        (add-hook 'org-mode-hook 'savefold-org--set-up-save-on-kill-buffer)
        (add-hook 'kill-emacs-hook 'savefold-org--save-all-buffers-folds)

        ;; Set up save folds on existing buffers
        (savefold-org--set-up-save-on-kill-for-existing-buffers))

    (remove-hook 'org-mode-hook 'savefold-org--recover-folds)

    (remove-hook 'org-mode-hook 'savefold-org--set-up-save-on-kill-buffer)
    (remove-hook 'kill-emacs-hook 'savefold-org--save-all-buffers-folds)

    (savefold-org--unhook-save-on-kill-for-existing-buffers)))

(provide 'savefold-org)

;;; savefold-org.el ends here
