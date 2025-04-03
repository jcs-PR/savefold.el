;;; test-savefold.el --- savefold.el tests -*- lexical-binding: t; -*-

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

;; savefold.el tests

;;; Code:

(require 'savefold)

(describe "savefold-mode"
  (before-all
    (setq saved-backends savefold-backends)
    (setq savefold-backends savefold--all-backends))

  (it "enables all backends when turned on"
    (savefold-mode 1)
    (expect (not
             (memq nil
                   (mapcar
                    (lambda (backend)
                      (symbol-value
                       (intern
                        (format "savefold-%s-mode" backend))))
                    savefold--all-backends)))))

  (it "disables all backends when turned off"
    (savefold-mode -1)
    (expect (not
             (memq nil
                   (mapcar
                    (lambda (backend)
                      (not
                       (symbol-value
                        (intern
                         (format "savefold-%s-mode" backend)))))
                    savefold--all-backends)))))

  (after-all
    (setq savefold-backends saved-backends)))

;;; test-savefold.el ends here
