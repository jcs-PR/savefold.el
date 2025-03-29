;;; savefold-test-utils.el --- shared utils for savefold tests -*- lexical-binding: t; -*-

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

;; Shared utilities for savefold tests.

;;; Code:

(defconst savefold-test-utils--dir (file-name-directory
                                    (expand-file-name load-file-name)))

(defmacro savefold-test-utils--with-open-file (fpath &rest body)
  "Evaluate BODY with FPATH as temporary current buffer."
  (declare (indent 1))
  (let ((buf (gensym)))
    `(let ((,buf (find-file-noselect ,fpath)))
       (with-current-buffer ,buf
         ,@body)
       (kill-buffer ,buf))))

(defmacro savefold-test-utils--with-temp-savefold-environment
    (source-fpath attr-fpath &rest body)
  "Create a new temporary savefold environment and evaluate BODY.

SOURCE-FPATH is a fixture file for the mode being tested, and the file at
ATTR-FPATH should contain the desired initial attribute hash table for that
file.

This macro anaphorically captures the symbol `temp-source-fpath' with the new
path of SOURCE-FPATH copied under the temporary directory."
  (declare (indent 2))
  (let ((temp-dir (gensym))
        (temp-attr-fpath (gensym)))
    `(let* ((,temp-dir (make-temp-file "test-savefold" t))
            (savefold-directory ,temp-dir) ;; not temp-dir/savefold
            (temp-source-fpath
             (expand-file-name (file-name-nondirectory ,source-fpath) ,temp-dir))
            (,temp-attr-fpath (savefold-utils--get-attr-table-fpath temp-source-fpath)))

       ;; Set up temporary savefold environment
       ;; This should set the file last-modified times to the current time.
       (copy-file ,source-fpath temp-source-fpath)
       (copy-file ,attr-fpath ,temp-attr-fpath)

       ;; Update the fixture 'savefold-modtime so that folds can be recovered
       ;; Force fundamental-mode, so as to not trigger any hooks.
       (let (auto-mode-alist)
         (savefold-test-utils--with-open-file temp-source-fpath
           (savefold-utils-set-file-attr-modtime)
           (savefold-utils-write-out-file-attrs)))

       ,@body

       ;; Clean up
       (delete-directory ,temp-dir t))))

(defun savefold-test-utils--sets-equalp (set1 set2)
  "Check whether two lists, considered as unordered sets, are the same.

Checks element equality with `equal'."
  (and (cl-subsetp set1 set2 :test 'equal)
       (cl-subsetp set2 set1 :test 'equal)))

(provide 'savefold-test-utils)

;;; savefold-test-utils.el ends here
