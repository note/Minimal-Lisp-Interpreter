(defun list (&rest rest) rest)

(defun not (app) (if app NIL T))

(defun mapcar (function list)
	   (if (car list)
	       (cons (funcall function (car list)) (mapcar function (cdr list)))))

(defun apply (fun params)
    (progn (setq params (mapcar (lambda (x) `(quote ,x)) params)) 
	   (eval `(funcall ,fun ,@params))))

(defmacro when (test &rest forms)
  `(if ,test (progn ,@forms)))

(defmacro unless (test &rest body)
  `(if (not ,test) (progn ,@body)))

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

;; arithmetic
(defmacro incf (var-name)
    `(setq ,var-name (+ ,var-name 1)))

(defun 1+ (value)
    (+ value 1))
    
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

(defun list-length (list)
	   (if (car list)
	       (1+ (list-length (cdr list)))
	       0))

(defun __append-two-lists (L1 L2)
	   (if (car L1)
	       (cons (car L1) (__append-two-lists (cdr L1) L2))
	       L2))

(defun append (&rest lists)
	   (if (> (list-length lists) 1)
	       (__append-two-lists (car lists) (apply #'append (cdr lists)))
	       (car lists)))
    
;; looping

(defun __do-initializer-list (var-list res)
	   (when (car var-list)
	     (cons (list (first (first var-list)) (second (first var-list))) (__do-initializer-list (cdr var-list) res))))

(defun __do-expressions-list (var-list res)
	   (when (car var-list)
            (if (third (first var-list))
	     (cons `(setq ,(first (first var-list)) ,(third (first var-list))) (__do-expressions-list (cdr var-list) res))
             (__do-expressions-list (cdr var-list) res))))

(defun __do (end-condition fun)
	   (unless (funcall end-condition)
	     (funcall fun)
	     (__do end-condition fun)))

(defmacro do (var-list end-list &rest body)
	   `(let ((fun-name (gensym)) (end-cond-name (gensym)))
	     (let ,(__do-initializer-list var-list (list))
	      (let ((fun-name (lambda () (progn ,@body ,@(__do-expressions-list var-list (list))))) (end-cond-name (lambda () ,(first end-list))))
		(progn (__do end-cond-name fun-name)
		       ,(second end-list))))))

(defmacro dotimes (header &rest body)
	   `(do ((,(first header) 0 (+ 1 ,(first header)))) ((= ,(first header) ,(second header)) ,(third header)) ,@body))

(defun __dolist (var-name list fun)
	   (when (car list)
	     (funcall fun list)
	     (__dolist var-name (cdr list) fun)))

(defmacro dolist (header &rest body)
	   `(let ((fun-name (gensym)))
             (let ((,(first header) 0))
	      (let ((fun-name (lambda (l) (progn
				       (setq ,(first header) (car l)) ,@body))))
				 (progn (__dolist ,(first header) ,(second header) fun-name)
					,(third header))))))
        
;; sequence functions