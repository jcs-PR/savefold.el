;;; savefold.el --- persistence for various folding systems -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jacob Fong
;; Author: Jacob Fong <jacobcfong@gmail.com>
;; Version: 0.0.1
;; Keywords: convenience
;; Homepage: https://github.com/jcfk/savefold.el

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

;; Persistence for emacs folding systems. See github.com/jcfk/savefold.el for
;; more information.

;;; Code:

(defgroup savefold nil
  "Custom group for savefold."
  :group 'convenience)

(defcustom savefold-backends '(outline)
  "List of folding backends to persist with savefold-mode.

See `savefold--all-backends' for a list of possible values."
  :type '(repeat symbol)
  :group 'savefold)

(defcustom savefold-directory (locate-user-emacs-file "savefold")
  "Persist fold data to this directory."
  :type 'directory
  :group 'savefold)

(defvar savefold--all-backends '(outline org origami)
  "List of supported folding backends.")

(defun savefold--enable-backends ()
  "Require and turn on the savefold minor mode for all `savefold-backends'."
  (mapc
   (lambda (backend)
     (let* ((backend-name (symbol-name backend))
            (feature (intern (format "savefold-%s" backend-name)))
            (minor-mode (intern (format "savefold-%s-mode" backend-name))))
       (require feature)
       (funcall minor-mode 1)))
   savefold-backends))

(defun savefold--disable-backends ()
  "Disable on the savefold minor mode for all `savefold-backends'."
  (mapc
   (lambda (backend)
     (let* ((backend-name (symbol-name backend))
            (minor-mode (intern (format "savefold-%s-mode" backend-name))))
       (funcall minor-mode -1)))
   savefold-backends))

;;;###autoload
(define-minor-mode savefold-mode
  "Toggle global folding persistence for `savefold-backends'."
  :global t
  :init-value nil
  (if savefold-mode
      (progn
        (savefold--enable-backends))
    (savefold--disable-backends)))

(provide 'savefold)

;;; savefold.el ends here
