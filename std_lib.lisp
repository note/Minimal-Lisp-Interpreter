(defun list (&rest rest) rest)

(defun not (app) (if app NIL T))

(defun apply (fun params)
    (eval `(funcall ,fun ,@params)))

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

(defun fff (var-list res)
	   (when (car var-list)
	     (cons (list (first (first var-list)) (second (first var-list))) (fff (cdr var-list) res))))

(defun ggg (var-list res)
	   (when (car var-list)
	     (cons `(setq ,(first (first var-list)) ,(third (first var-list))) (ggg (cdr var-list) res))))

(defun __do (end-condition fun)
	   (unless (funcall end-condition 0)
	     (funcall fun 0)
	     (__do end-condition fun)))

(defmacro do (var-list end-list &rest body)
	   `(let ,(fff var-list (list))
	      (let ((fun (lambda (a) (progn ,@body ,@(ggg var-list (list))))) (end-cond (lambda (y) ,(first end-list))))
		(progn (__do end-cond fun)
		       ,(second end-list)))))

(defmacro dotimes (header &rest body)
	   `(do ((,(first header) 0 (+ 1 ,(first header)))) ((= ,(first header) ,(second header)) ,(third header)) ,@body))

(defun __dolist (var-name list fun)
	   (when (car list)
	     (funcall fun list)
	     (__dolist var-name (cdr list) fun)))

(defmacro dolist (header &rest body)
	   `(let ((,(first header) 0))
	      (let ((fun (lambda (l) (progn
				       (setq ,(first header) (car l)) ,@body))))
				 (progn (__dolist ,(first header) ,(second header) fun)
					,(third header)))))
        
;; sequence functions