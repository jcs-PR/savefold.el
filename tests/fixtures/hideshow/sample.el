;; The purpose of this document is to facilitate testing of savefold-hideshow.

;; Recovery testing:

(defun first-test-function (arg1 arg2)
  ;; The following expression starts closed and ends closed
  (concat "lorem ipsum dolor"
          " sit amet"))

;; Fold save testing:

;; The following expression starts open and ends closed
(defun second-test-function (arg1 arg2)
  (concat "lorem ipsum dolor"
          " sit amet"))

;; This comment starts open and ends closed.
;;
;; Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nulla cursus tellus
;; at felis cursus eleifend. Mauris et tincidunt turpis. Suspendisse justo
;; magna, ornare a mollis id, imperdiet eget urna. Donec ac facilisis nisl. Cras
;; commodo rhoncus fermentum. Sed ac suscipit mauris. Nulla vel odio mauris.
