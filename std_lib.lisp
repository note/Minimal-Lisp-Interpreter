(defun list (&rest rest) rest)

(defun not (x) (if x NIL T))

(defun apply (fun params)
    (eval `(funcall ,fun ,@params)))

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

;;(defmacro dotimes (header &rest body)
;;    )
    
;; list functions
(defun nth (index list)
    (if (car list)
        (if (= index 0)
            (car list)
            (nth (- index 1) (cdr list)))
        NIL))
        
(defun first (list)
    (nth 0 list))
    
(defun second (list)
    (nth 1 list))

(defun third (list)
    (nth 2 list))
    
;; looping

(defun __dotimes (val high-val fun)
	   (when (< val high-val)
	     (funcall fun val)
	     (__dotimes (+ val 1) high-val fun)))

(defmacro dotimes (header body)
	   `(let ((fun (lambda (,(first header)) ,body)))
		(progn (__dotimes 0 ,(second header) fun) 
		       ,(third header))))
        
;; sequence functions