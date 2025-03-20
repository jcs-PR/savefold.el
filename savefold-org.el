;;; savefold-org.el --- savefold for org mode -*- lexical-binding: t; -*-

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

(defvar-local savefold-org--folds '()
  "A list of org mode fold data for the current buffer's file.

A list of lists (start end spec) for each org fold overlay, corresponding to the
arguments of `org-flag-region'.")

(defvar savefold-org--org-fold-specs '(org-hide-block))

(defun savefold-org--recover-folds ()
  "Read and apply saved org fold data for the current buffer."
  (when (and (not savefold-org-inhibit-outline-integration)
             (not savefold-outline-mode))
    (savefold-outline--recover-folds))
  (mapc
   (lambda (fold-data)
     (org-flag-region (car fold-data) (cadr fold-data) t (caddr fold-data)))
   (savefold-utils-get-file-attr 'savefold-org--folds)))

(defun savefold-org--setup-save-on-kill-buffer ()
  (add-hook 'kill-buffer-hook 'savefold-org--save-folds nil t))

(defun savefold-org--org-foldp (ov)
  "Check whether the overlay is for an org-mode fold.

This does not include folds made with `org-flag-region' with an 'outline
invisibility spec, but only the invisibility specs exclusive to org-mode:
`savefold-org--org-fold-specs'."
  (memq (overlay-get ov 'invisible) savefold-org--org-fold-specs))

(defun savefold-org--save-folds ()
  "Save org fold data for the current buffer."
  (when (and (not savefold-org-inhibit-outline-integration)
             (not savefold-outline-mode))
    (savefold-outline--save-folds))
  (setq
   savefold-org--folds
   (mapcar
    (lambda (ov)
      `(,(overlay-start ov) ,(overlay-end ov) ,(overlay-get ov 'invisible)))
    (seq-filter 'savefold-org--org-foldp
                (overlays-in (point-min) (point-max)))))
  (savefold-utils-set-file-attr 'savefold-org--folds
                                savefold-org--folds)
  (savefold-utils-write-out-file-attrs))

(defun savefold-org--save-all-buffers-folds ()
  "Save fold data across all org buffers."
  (mapc
   (lambda (buf)
     (with-current-buffer buf
       (when (derived-mode-p 'org-mode)
         (savefold-org--save-folds))))
   (buffer-list)))

(define-minor-mode savefold-org-mode
  "Toggle global persistence for org-mode folds."
  :global t
  :init-value nil
  (if savefold-org-mode
      (progn
        ;; Recover folds upon file open
        (add-hook 'org-mode-hook 'savefold-org--recover-folds)

        ;; Save folds on file close
        (add-hook 'org-mode-hook 'savefold-org--setup-save-on-kill-buffer)

        ;; Save existing folds
        (savefold-org--save-all-buffers-folds))
    (remove-hook 'org-mode-hook 'savefold-org--recover-folds)
    (remove-hook 'org-mode-hook 'savefold-org--setup-save-on-kill-buffer)))

(provide 'savefold-org)

;;; savefold-org.el ends here
