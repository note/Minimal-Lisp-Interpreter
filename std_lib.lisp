(defun list (&rest rest) rest)

(defmacro when (test &rest forms) `(if ,test (progn ,@forms)))