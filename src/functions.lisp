;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			      Special Functions 
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; $Header: functions.lisp,v 1.3 1995/07/07 13:40:57 mardis Exp $


(in-package "WEYLI")

;; Symbolic Lambda expressions

(defun make-app-function (args body)
  (let* ((domain (domain-of body))
         (old-vars (mapcar #'(lambda (x) (coerce x domain)) args))
         (new-vars nil))
    (loop for i upfrom 1
          for v in old-vars
          for new-var =  (coerce (intern (format nil "V.~D" i)) domain)
          do (cond ((ge-equal new-var v)
                    (push v new-vars))
                   (t (setq body (substitute new-var v body))
                      (push new-var new-vars))))
    (setq new-vars (reverse new-vars))
    (when (typep body 'general-expression)
      (setq body (expand body)))
    (cond ((and (typep body 'ge-application)
		(loop for v in new-vars
		      for a in (args-of body)
		      do (when (not (ge-equal a v))
			   (return nil))
		      finally (return t)))
	   (funct-of body))
	  (t (make-instance 'applicable-function 
			    :domain domain
			    :bound-vars new-vars
			    :nargs (length args)
			    :body body)))))

(defmethod print-object ((fun applicable-function) stream)
  (format stream "(lambda ~S ~S)" (bound-vars-of fun) (body-of fun)))

(defmethod apply ((fun applicable-function) &rest args)
  (setq args (accum-apply-args args))
  (unless (eql (nargs-of fun) (length args))
    (error "Argument lengths don't match"))
  (let* ((body (body-of fun))
         (domain (domain-of body)))
    (loop for arg in args
          for v in (bound-vars-of fun)
          do (setq body (substitute (coerce arg domain) v body)))
    body))

(defun canonicalize-functions (x y)
  (unless (eql (nargs-of x) (nargs-of y))
    (error "Two functions have different number of arguments"))
  (let ((x-body (body-of x))
        (y-body (body-of y)))
    (loop for x-arg in (bound-vars-of x)
          for y-arg in (bound-vars-of y)
          do (unless (ge-equal x-arg y-arg)
               (setq y-body (substitute x-arg y-arg y-body))))
    (values (bound-vars-of x) x-body y-body)))

(defmacro define-applicable-function-binary (operator)
  `(progn
     (defmethod ,operator ((x applicable-function) y)
       (make-app-function (bound-vars-of x) (,operator (body-of x) y)))

     (defmethod ,operator (x (y applicable-function))
       (make-app-function (bound-vars-of y) (,operator x (body-of y))))

     (defmethod ,operator ((x applicable-function) (y domain-element))
       (make-app-function (bound-vars-of x) (,operator (body-of x) y)))

     (defmethod ,operator ((x domain-element) (y applicable-function))
       (make-app-function (bound-vars-of y) (,operator x (body-of y))))

     (defmethod-sd ,operator ((x applicable-function) (y applicable-function))
       (multiple-value-bind (args x-body y-body) (canonicalize-functions x y)
	 (make-app-function args (,operator x-body y-body))))

     ;; FIXTHIS: Why do the following two methods need to be :around methods?
     ;; The :around is only needed for MCL!!
     (defmethod ,operator #+MCL :around ((x applicable-function) (y ge-function)) 
       (unless (= (nargs-of x) (nargs-of y))
	 (error "Can't add two functions with different arity: ~S ~S"
		x y))
       (make-app-function (bound-vars-of x)
			  (,operator (body-of x) (apply y (bound-vars-of x)))))

     (defmethod ,operator #+MCL :around ((y ge-function) (x applicable-function)) 
       (unless (= (nargs-of x) (nargs-of y))
	 (error "Can't add two functions with different arity: ~S ~S"
		x y))
       (make-app-function (bound-vars-of x)
			  (,operator (body-of x) (apply y (bound-vars-of x)))))

     (defmethod ,operator ((x ge-function) y) 
       (make-app-function '(%temp%) (,operator (funcall x '%temp%) y)))

     ;; This is needed because of precidence problems.
     ;; (number domain-elt) comes before (t ge-function)
     (defmethod ,operator ((x (or t number numeric)) (y ge-function))
       (make-app-function '(%temp%) (,operator x (funcall y '%temp%))))

     (defmethod-sd ,operator ((x ge-function) (y domain-element))
       (make-app-function '(%temp%) (,operator (funcall x '%temp%) y)))

     (defmethod-sd ,operator ((x domain-element) (y ge-function))
       (make-app-function '(%temp%) (,operator x (funcall y '%temp%))))

     (defmethod-sd ,operator ((x ge-function) (y ge-function))
       (unless (= (nargs-of x) (nargs-of y))
	 (error "Can't combine two functions with different arity: (~S ~S ~S)"
		',operator x y))
       (make-app-function '(%temp%)
	    (,operator (funcall x '%temp%) (funcall  y '%temp%))))))

(defmethod 0? ((x applicable-function))
  (0? (body-of x)))

(defmethod 1? ((x applicable-function))
  (1? (body-of x)))

(define-applicable-function-binary plus)
(define-applicable-function-binary difference)

(defmethod minus ((x ge-function))
  (make-app-function '(%temp%) (minus (funcall x '%temp%))))

(defmethod minus ((x applicable-function))
  (make-app-function (bound-vars-of x) (minus (body-of x))))

(define-applicable-function-binary times)
(define-applicable-function-binary quotient)

(defmethod recip ((x ge-function))
  (make-app-function '(%temp%) (recip (funcall x '%temp%))))

(defmethod recip ((x applicable-function))
  (make-app-function (bound-vars-of x) (recip (body-of x))))

(defmethod expt ((x applicable-function) (y (or number symbol)))
  (make-app-function (bound-vars-of x) (expt (body-of x) y)))

(defmethod-sd expt ((x applicable-function) (y domain-element))
  (if (not (typep y 'abstract-function))
      (make-app-function (bound-vars-of x) (expt (body-of x) y))
      (call-next-method)))

(defmethod expt ((x ge-function) (y (or number symbol)))
  (make-app-function '(%temp%) (expt (funcall x '%temp%) y)))

(defmethod-sd expt ((x ge-function) (y domain-element))
  (if (not (typep y 'abstract-function))
      (make-app-function '(%temp%) (expt (funcall x '%temp%) y))
      (call-next-method)))

(defmethod deriv ((exp applicable-function) &rest vars)
  (make-app-function (bound-vars-of exp)
     (apply #'deriv (cons (body-of exp)
        (loop for v in vars
	      collect (cond ((typep v 'symbol) v)
			    ((typep v 'ge-variable) v)
	                    ((and (typep v 'integer)
				  (not (minusp v))
				  (< v (nargs-of exp)))
			      (elt (bound-vars-of exp) v))
			    (t (error "Cannot take deriv of ~A and ~A"
				      exp v))))))))

;; Special functions

(defmacro def-ge-1oper (name (arg))
  (let ((maker-name (intern (format nil "MAKE-GE-~A" (string name))))
        (predicate-name (intern (format nil "GE-~A?" (string name)))))
    `(progn
       (make-function nil ',name 1)
       (defun ,maker-name (domain ,arg)
         (make-ge-funct domain (make-function domain ',name 1) ,arg))
       (defun ,predicate-name (arg)
         (and (ge-application? arg)
              (ge-function? (funct-of arg))
              (string-equal (name-of (funct-of arg)) ,(string name))))
       (defmethod ,name ((,arg symbol))
	 (simplify (,maker-name *general* (coerce ,arg *general*))))
       (defmethod ,name ((,arg numeric))
	 (let* ((arg ,arg)
		domain)
	   (cond ((typep (domain-of arg) 'general-expressions)
		  (setq domain (domain-of arg)))
		 (t (setq domain *general*)
		    (setq arg (coerce arg domain))))
	 (simplify (,maker-name domain arg))))
       (defmethod ,name ((,arg general-expression))
	 (simplify (,maker-name (domain-of ,arg) ,arg)))
       )))

(defmacro def-ge-2oper (name (x y))
  (let ((maker-name (intern (format nil "MAKE-GE-~A" (string name))))
        (predicate-name (intern (format nil "GE-~A?" (string name)))))
    `(progn
	 (make-function nil ',name 2)
       (defun ,maker-name (domain ,x ,y)
         (make-ge-funct domain (make-function domain ',name 2) ,x ,y))
       (defun ,predicate-name (arg)
         (and (ge-application? arg)
              (ge-function? (funct-of arg))
              (string-equal (name-of (funct-of arg)) ,(string name))))
       (defmethod ,name (,x ,y)
         (simplify (,maker-name *general* (coerce ,x *general*)
				(coerce ,y *general*)))))
       ))

(defmacro def-ge-3oper (name (a i1 i2))
  (let ((maker-name (intern (format nil "MAKE-GE-~A" (string name))))
        (predicate-name (intern (format nil "GE-~A?" (string name)))))
    `(progn
	 (make-function nil ',name 3)
       (defun ,maker-name (domain ,a ,i1 ,i2)
         (make-ge-funct domain (make-function domain ',name 3) ,a ,i1 ,i2))
       (defun ,predicate-name (arg)
         (and (ge-application? arg)
              (ge-function? (funct-of arg))
              (string-equal (name-of (funct-of arg)) ,(string name))))
       (defmethod ,name (,a ,i1 ,i2)
         (simplify (,maker-name *general* (coerce ,a *general*)
				(coerce ,i1 *general*)
				(coerce ,i2 *general*))))
       )))
   
(defmacro defsimplify-funct (name args &body body)
  (let ((simp-name (intern (format nil "SIMPF-~A" name))))
    `(progn
       (defun ,simp-name ,args ,@body)
       (setf (getf (make-function nil ',name) 'simplify) ',simp-name))))

(defmacro defderiv-funct (name &body body)
  (let ((fun-name (intern (format nil ".~A-deriv." name))))
    `(progn
       (defun ,fun-name ()
	 (setf (getf (make-function nil ',name) 'deriv) 
	       (list ,@body)))
       (pushnew ',fun-name *initialize-contexts-funs*))))

(def-ge-1oper ABS (x))
(def-ge-1oper REALPART (x))
(defsimplify-funct realpart (domain whole exp)
  (declare (ignore domain))
  (cond ((or (ge-abs? exp) 
             (ge-realpart? exp)
             (ge-imagpart? exp))
         exp)
        (t whole)))

(def-ge-1oper IMAGPART (x))
(defsimplify-funct imagpart (domain whole exp)
  (cond ((or (ge-abs? exp) 
             (ge-realpart? exp)
             (ge-imagpart? exp))
         (zero domain))
        (t whole)))

(def-ge-1oper LOG (x))
(defsimplify-funct log (domain whole exp)
  (cond ((cl:floatp exp)
         (make-element domain (cl:log exp)))
        ((ge-expt? exp)
         (simplify
          (make-ge-times domain
                         `(,(exponent-of exp)
                           ,(make-ge-log (domain-of exp) (base-of exp))))))
        (t whole)))

(defderiv-funct log
  (make-app-function '(x) (expt 'x -1)))

(def-ge-1oper SIN (x))
(defsimplify-funct sin (domain whole exp)
  (cond ((cl:floatp exp)
         (make-element domain (cl:sin exp)))
        ((and (number? exp) (0? exp))
         (make-element domain 0))
        ((ge-minus? exp)
         (- (make-ge-sin domain (- exp))))
        (t whole)))

(defderiv-funct sin
  (make-app-function  '(x) (cos 'x)))

(def-ge-1oper COS (x))
(defsimplify-funct cos (domain whole exp)
  (cond ((cl:floatp exp)
         (make-element domain (cl:cos exp)))
        ((and (number? exp) (0? exp))
         (make-element domain 1))
        ((ge-minus? exp)
         (make-ge-cos domain (- exp)))
        (t whole)))

(defderiv-funct cos
    (make-app-function '(x) (- (sin 'x))))

(def-ge-1oper TAN (x))
(defsimplify-funct tan (domain whole exp)
    (cond ((cl:floatp exp)
	   (make-element domain (cl:tan exp)))
	  ((and (number? exp) (0? exp))
	   (make-element domain 0))
	  ((ge-minus? exp)
	   (- (make-ge-tan domain (- exp))))
	  (t whole)))

(defderiv-funct tan
  (make-app-function '(x) (- (expt (cos 'x) -2))))

(def-ge-1oper ASIN (x))
(defsimplify-funct asin (domain whole exp)
  (cond ((cl:floatp exp)
         (make-element domain (cl:asin exp)))
        ((and (number? exp) (0? exp))
         (make-element domain 0))
        ((ge-minus? exp)
         (- (make-ge-asin domain (- exp))))
        (t whole)))

(defderiv-funct asin
  (make-app-function '(x) (- (expt (- 1 (expt 'x 2)) -1/2))))

(def-ge-1oper ACOS (x))
(defsimplify-funct acos (domain whole exp)
  (cond ((cl:floatp exp)
         (make-element domain (cl:acos exp)))
        (t whole)))

(defderiv-funct acos
  (make-app-function '(x) (expt (- 1 (expt 'x 2)) -1/2)))

;; These are conditionalized out since, we might want to have a two
;; argument version of atan!.
#+FIXTHIS
(def-ge-1oper ATAN (x))
#+FIXTHIS
(defsimplify-funct atan (domain whole exp)
  (cond ((cl:floatp exp) (cl:atan exp))
	  (t `(atan ,exp))))

#+FIXTHIS
(defderiv-funct atan
  (make-app-function '(x) (/ (+ 1 (expt 'x 2)))))

(def-ge-1oper SINH (x))
(defsimplify-funct sinh (domain whole exp)
  (cond ((cl:floatp exp)
         (make-element domain (cl:sinh exp)))
        ((and (number? exp) (0? exp))
         (make-element domain 0))
        ((ge-minus? exp)
         (- (make-ge-sinh domain (- exp))))
        (t whole)))

(defderiv-funct sinh 
  (make-app-function '(x) (cosh 'x)))

(def-ge-1oper COSH (x))
(defsimplify-funct cosh (domain whole exp)
  (cond ((cl:floatp exp)
         (make-element domain (cl:cosh exp)))
        ((and (number? exp) (0? exp))
         (make-element domain 1))
        ((ge-minus? exp)
         (make-ge-cosh domain (- exp)))
        (t whole)))

(defderiv-funct cosh
  (make-app-function '(x) (sinh 'x)))

(def-ge-1oper TANH (x))
(defsimplify-funct tanh (domain whole exp)
  (cond ((cl:floatp exp)
         (make-element domain (cl:tanh exp)))
        ((and (number? exp) (0? exp))
         (make-element domain 0))
        ((ge-minus? exp)
         (- (make-ge-tanh domain (- exp))))
        (t whole)))

(defderiv-funct tanh
  (make-app-function '(x) (expt (cosh 'x) -2)))

(def-ge-1oper ASINH (x))
(defsimplify-funct asinh (domain whole exp)
  (cond ((cl:floatp exp)
         (make-element domain (cl:asinh exp)))
        ((and (number? exp) (0? exp))
         (make-element domain 0))
        ((ge-minus? exp)
         (- (make-ge-asinh domain (- exp))))
        (t whole)))

(defderiv-funct asinh 
  (make-app-function '(x) (expt (+ 1 (expt 'x 2)) -1/2)))

(def-ge-1oper ACOSH (x))
(defsimplify-funct acosh (domain whole exp)
  (cond ((cl:floatp exp)
         (make-element domain (cl:acosh exp)))
        (t whole)))

(defderiv-funct acosh 
  (make-app-function '(x) (expt (+ 1 (expt 'x 2)) -1/2)))

#+FIXTHIS
(def-ge-1oper ATANH (x))
#+FIXTHIS
(defsimplify atanh (domain whole exp)
  (cond ((cl:floatp exp) (cl:atanh exp))
	  (t `(atanh ,exp))))

#+FIXTHIS
(defderiv-funct atanh
  (make-app-funciton '(x) (expt (- 1 (expt 'x 2)) -1)))


(def-ge-3oper aref2 (a i1 i2))

(def-ge-2oper aref1 (a i1))

(def-ge-2oper max-pair (x y))
(defsimplify-funct max-pair (domain whole e1 e2)
  (declare (ignore domain))
  (cond ((ge-equal e1 e2)
	 e1)
	((ge-equal e1 *negative-infinity*)
	 e2)
	((ge-equal e2 *negative-infinity*)
	 e1)
	((and (number? e1)
	      (number? e2))
         (max e1 e2))
	(t whole)))

(def-ge-2oper min-pair (x y))
(defsimplify-funct min-pair (domain whole e1 e2)
  (declare (ignore domain))
  (cond ((ge-equal e1 e2)
	 e1)
	((ge-equal e1 *positive-infinity*)
	 e2)
	((ge-equal e2 *positive-infinity*)
	 e1)
	((and (number? e1)
	      (number? e2))
         (min e1 e2))
	(t whole)))
