(defun list (&rest rest) rest)
(defun not (x) (if x NIL T))

(defmacro when (test &rest forms) `(if ,test (progn ,@forms)))