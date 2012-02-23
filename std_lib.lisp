(defun list (&rest rest) rest)
(defun not (x) (if x NIL T))

(defmacro when (test &rest forms)
  `(if ,test (progn ,@forms)))

(defmacro unless (condition &rest body)
  `(if (not ,condition) (progn ,@body)))

(defun __reverse (l res) (if (car l) (__reverse (cdr l) (cons (car l) res)) res))

(defun reverse (l)
    (__reverse l (list)))
    
(defun __and (expressions)
    (if (car expressions)
    (if (eval (car expressions))
        (__and (cdr expressions))
        NIL)
    T))
  
(defmacro and (&rest expressions)
    (__and expressions))