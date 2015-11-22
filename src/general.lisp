;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			      General Representation
;;; ===========================================================================
;;; (c) Copyright 1989, 1997 Cornell University

;;; $Header: general.lisp,v 1.15 1995/07/07 13:40:58 mardis Exp $

(in-package "WEYLI")

(defmethod set-memoization ((domain has-memoization) key value)
  (with-slots (memos) domain
    (setf (gethash key memos) value)
    value))

(defmethod get-memoization ((domain has-memoization) key)
  (with-slots (memos) domain
    (gethash key memos)))

(defsetf get-memoization set-memoization)

(defmacro %memoize (domain expression &body body)
  `(let ((.expr. ,expression))
    (with-slots (memos) ,domain
       (multiple-value-bind (value found?) (gethash .expr. memos)
	 (if found? value
	     (setf (get-memoization ,domain .expr.) (progn ,@body)))))))

(defmacro memoize (expression &body body)
  `(%memoize *general* ,expression ,@body))

#+ignore
(defun fib-memo (n)
  (memoize `(fib ,n)
    (if (< n 2) 1
	(+ (fib-memo (- n 1)) (fib-memo (- n 2))))))

(defmethod display ((ge general-expression) &optional stream &rest ignore)
  (declare (ignore ignore))
  (princ ge stream))

(defmethod simplify ((ge general-expression))
  ge)

(defmethod ge-equal (x y)
  (declare (ignore x y))
  nil)

(defmethod ge-equal ((x general-expression) (y general-expression))
  (eql x y))

(defmethod ge-great ((x general-expression) (y general-expression))
  nil)


;; Numbers and Variables

(defmethod make-quotient-element
    ((domain general-expressions) (x integer) (y integer))
  (make-instance 'rational-number :domain domain
		 :numerator x :denominator y))

(defmethod make-element ((domain general-expressions) (x integer) &rest args)
  (cond ((or (null args) (0? (first args)))
	 (make-instance 'rational-integer :domain domain :value x))
	((number? (first args))
	 (make-instance 'complex-number :domain domain
			:realpart x :imagpart (first args)))
	(t (error "Can't deal with this yet: ~A" args))))

(defmethod make-element ((domain general-expressions) (x ratio) &rest args)
  (cond ((or (null args) (0? (first args)))
	 (make-instance 'rational-number :domain domain
			:numerator (cl:numerator x)
			:denominator (cl:denominator x)))
	((number? (first args))
	 (make-instance 'complex-number :domain domain
			:realpart x :imagpart (first args)))
	(t (error "Can't deal with this yet: ~A" args))))

(defmethod make-element ((domain general-expressions) (x float) &rest args)
  (cond ((or (null args) (0? (first args)))
	 (make-instance 'floating-point-number :domain domain
			:value x))
	((number? (first args))
	 (make-instance 'complex-number :domain domain
			:realpart x :imagpart (first args)))
	(t (error "Can't deal with this yet: ~A" args))))

(defmethod make-element ((domain general-expressions) (x cl:complex)
			 &rest ignore)
  (declare (ignore ignore))
  (make-instance 'complex-number :domain domain
		 :realpart (cl:realpart x)
		 :imagpart (cl:imagpart x)))

(defmethod coerce ((num number) (domain general-expressions))
  (make-element domain num))

(defmethod coerce ((num rational-integer) (domain general-expressions))
  (if (eql (domain-of num) domain) num
      (make-element domain (integer-value num))))

(defmethod coerce ((num rational-number) (domain general-expressions))
  (if (eql (domain-of num) domain) num
      (make-instance 'rational-number :domain domain 
                     :numerator (qo-numerator num)
                     :denominator (qo-denominator num))))

(defmethod coerce ((num floating-point-number) (domain general-expressions))
  (if (eql (domain-of num) domain) num
      (make-element domain (fp-value num))))

(defmethod coerce ((num bigfloat) (domain general-expressions))
  (if (eql (domain-of num) domain) num
      (make-bigfloat domain (bigfloat-mantissa num) (bigfloat-exponent num))))

(defmethod coerce ((num cl:complex) (domain general-expressions))
  (make-instance 'complex-number :domain domain
		 :realpart (realpart num)
		 :imagpart (imagpart num)))

(defmethod coerce ((num complex-number) (domain general-expressions))
  (if (eql (domain-of num) domain) num
      (make-instance 'complex-number :domain domain
		     :realpart (cn-realpart num)
		     :imagpart (cn-imagpart num))))

(defmethod simplify ((x number))
  (make-element *general* x))

(defmethod simplify ((x numeric))
  x)

(defmethod ge-equal ((x (or number numeric)) (y (or number numeric)))
  (= x y))

(defmethod ge-equal ((x (or number numeric)) y)
  (declare (ignore y))
  nil)

(defmethod ge-equal (x (y (or number numeric)))
  (declare (ignore x))
  nil)

(defmethod ge-great ((x (or number numeric)) (y (or number numeric)))
  (> x y))

(defmethod ge-great ((x (or number numeric)) y)
  (declare (ignore y))
  nil)

(defmethod ge-great (x (y (or number numeric)))
  (declare (ignore x))
  t)

;; Variables

(defmethod initialize-instance :after ((var ge-variable) &rest ignore)
  (declare (ignore ignore))
  (reparse-print-string var))

(defmethod reparse-print-string ((var ge-variable))
  (let ((string (cond ((atom (symbol-of var))
		       (string-downcase (symbol-of var)))
		      (t (format nil "[~A]" (symbol-of var)))))
	temp)
    (when (setq temp (getf var :subscripts))
      (setq string 
	    (format nil "~A(~S~{,~S~})"
		    string (first temp) (rest temp))))
    (setf (string-of var) string)))

(defmethod coerce ((var symbol) (domain general-expressions))
  (make-ge-variable domain var))

(defmethod make-ge-variable ((domain general-expressions) var)
  (loop for v in (ge-variables domain)
	do (when (equal (symbol-of v) var)
	     (return v))
	finally
         (setq var (make-instance 'ge-variable :domain domain :symbol var))
	 (push var (ge-variables domain))
	 (return var)))

(defmethod print-object ((var ge-variable) stream)
  (let ((sym (string-of var)))
    (cond ((and (not (null sym)) (atom sym))
	   #+Genera
	   (format stream "~'i~A~" sym)
	   #-Genera
	   (princ sym stream))
	  (t (princ (symbol-of var) stream)))))

;; This function is only to be applied to general expressions. 
(defsubst ge-variable? (x)
  (typep x 'ge-variable))

(defmethod add-subscripts ((var symbol) &rest subscripts)
  (%apply #'add-subscripts (coerce var *general*) subscripts))

(defmethod add-subscripts ((var ge-variable) &rest subscripts)
  (setq var (coerce var *general*))
  (let* ((symbol (symbol-of var))
	 (subscripts (append (getf var :subscripts) (copy-list subscripts)))
	 (canonical-var 
	  (member symbol (ge-variables *general*)
		  :test #'(lambda (a b)
			    (and (equal a (symbol-of b))
				 (equal subscripts (getf b :subscripts)))))))
    (cond (canonical-var
	   (first canonical-var))
	  (t (setq var (make-instance 'ge-variable :domain (domain-of var)
				      :symbol symbol))
	     (setf (getf var :subscripts) subscripts)
	     (reparse-print-string var)
	     (push var (ge-variables *general*))
	     var))))

(defmethod ge-equal ((x ge-variable) (y ge-variable))
  (eql x y))

(defmethod ge-great ((x ge-variable) (y ge-variable))
  (string-greaterp (string-of x) (string-of y)))

(defmethod ge-great ((x ge-variable) (y (or ge-plus ge-times)))  
  (loop for w in (terms-of y)
	unless (ge-great x w)
	  do (return nil)
	finally (return t)))

(defmethod ge-great ((x (or ge-plus ge-times)) (y ge-variable))  
  (loop for w in (terms-of x)
	unless (ge-great w y)
	  do (return t)
	finally (return nil)))

;; Functions

(defun search-for-function (list name nargs)
  (loop for f in list
        do (when (and (not (typep f 'ge-function-deriv))
                      (string= name (name-of f)))
             (when (and nargs (not (cl:= nargs (nargs-of f))))
               (error "Wrong number of arguments specified for function ~A"
                      name))
             (return f))))  

(defmethod get-function ((domain general-expressions) name &optional nargs)
  (setq name (string-downcase (string name)))
  (or (search-for-function (ge-functions domain) name nargs)
      (search-for-function *global-functions* name nargs)))

(defmethod get-function ((domain (eql nil)) name &optional nargs)
  (setq name (string-downcase (string name)))
  (search-for-function *global-functions* name nargs))

(defmethod make-function ((domain general-expressions) name &optional nargs)
  (setq name (string-downcase (string name)))
  (let ((fun (or (search-for-function (ge-functions domain) name nargs)
                 (search-for-function *global-functions* name nargs))))
    (unless fun
      (when (null nargs)
        (error "Number of arguments to ~A must be specified" name))
      (setq fun (make-instance 'ge-function :domain domain
                               :name name :nargs nargs))
      (push fun (ge-functions domain)))
    fun))

(defmethod make-function ((domain (eql nil)) name &optional nargs)
  (setq name (string-downcase (string name)))
  (let ((fun (search-for-function *global-functions* name nargs)))
    (unless fun
      (when (null nargs)
        (error "Number of arguments to ~A must be specified" name))
      (setq fun (make-instance 'ge-function :domain domain
                               :name name :nargs nargs))
      (push fun *global-functions*))
    fun))

(defmethod derivs-of ((f ge-function))
  nil)

(defun add-function-to-domain (domain name nargs &optional derivs)
  (let ((function-class (if derivs 'ge-function-deriv 'ge-function))
        deriv)
    (loop for f in (ge-functions domain)
          do (when (and (typep f function-class)
                        (eql (name-of f) name)
                        (equal (derivs-of f) derivs))
               (setq deriv f)
               (return t)))
    (unless deriv
      (setq deriv (make-instance function-class :domain domain
                                 :name name :nargs nargs
                                 :derivs derivs))
      (push deriv (ge-functions domain)))
    deriv))

;; The copy-list's in the following functions is necessary because
;; sort destructively modifies its argument.  --RZ
(defmethod make-function-deriv ((fun ge-function) (i integer))
  (when (or (minusp i)
            (not (< i (nargs-of fun))))
    (error "Illegal derivative of ~S in position ~D" fun i))
  (add-function-to-domain (domain-of fun)
                          (name-of fun)
                          (nargs-of fun)
                          (if (typep fun 'ge-function-deriv)
                            (sort (cons i (copy-list (derivs-of fun)))
                                  #'cl:<)
                            (list i))))

(defmethod make-function-deriv ((fun ge-function) (derivs list))
  (add-function-to-domain (domain-of fun)
                          (name-of fun)
                          (nargs-of fun)
			  (sort (if (typep fun 'ge-function-deriv)
				    (append derivs (copy-list (derivs-of fun)))
				    derivs)
				#'cl:<)))

(defmethod make-function-integrate ((fun ge-function) (i integer))
  (when (or (minusp i)
            (not (< i (nargs-of fun))))
    (error "Illegal derivative of ~S in position ~D" fun i))
  (let ((derivs (if (typep fun 'ge-function-deriv)
                  (derivs-of fun) nil)))
    (cond ((member i derivs)
           (setq derivs (remove i derivs :count 1)))
          (t (error "Don't have representation for integrals yet")))
    (add-function-to-domain (domain-of fun) (name-of fun) (nargs-of fun)
                            derivs)))

(defmethod print-object ((fun ge-function) stream)
  #+Genera
  (format stream "~'i~A~" (name-of fun))
  #-Genera
  (princ (name-of fun) stream))

(defmethod print-object ((fun ge-function-deriv) stream)
  #+Genera
  (format stream "~'i~A~" (name-of fun))
  #-Genera
  (princ (name-of fun) stream)
  (princ "_{" stream)
  (loop for n in (derivs-of fun)
        do (princ n stream))
  (princ "}" stream))

(defmethod make-ge-funct ((domain general-expressions) funct &rest args)
  (make-instance 'ge-application :domain domain
		 :funct (if (ge-function? funct) funct
                            (make-function domain funct (length args)))
                 :args (copy-list args)))

(defmethod apply ((fun ge-function) &rest args)
  (let ((domain (domain-of fun)))
    (flet ((check-domain (dom)
             (cond ((null domain)
                    (if (typep dom 'general-expressions)
                      (setq domain dom)
                      (error "GE function of ~D" dom)))
                   ((not (eql domain dom))
                    (error "Incompatible domains apply ~S to ~S" fun args)))))
      (setq args (accum-apply-args args))
      (loop for arg in args
            do (when (or (typep arg 'general-expression)
                         (typep arg 'numeric))
                 (check-domain (domain-of arg))))
      (when (null domain)
        (setq domain *general*))
      (simplify
       (make-instance 'ge-application :domain domain
                      :funct fun
                      :args (loop for arg in args
                                  collect (coerce arg domain)))))))

(defmacro funct (function &rest args)
  `(make-ge-funct *general* ',function
      ,@(mapcar #'(lambda (q) `(coerce ,q *general*))
		args)))

(defmethod print-object ((x ge-application) stream)
  (print-object (funct-of x) stream)
  (write-char #\( stream)
  (display-list (args-of x) stream)
  (write-char #\) stream))

(defmethod simplify ((x ge-application))
  (let ((args (mapcar #'simplify (args-of x)))
        (simplifier (getf (funct-of x) 'simplify))
	new-x)
    ;; This is the function application with the simplified arguments.
    (setq new-x (apply #'make-ge-funct (domain-of x) (funct-of x) args))
    (if simplifier
      (apply simplifier (domain-of x) new-x args)
      new-x)))

;; Contexts

(defvar *initialize-contexts-funs* ())

(defun initialize-contexts ()
  (setq *general* (make-instance 'general-expressions))
  (loop for fun in *initialize-contexts-funs*
	do (funcall fun)))

(defmacro with-new-context (&body body)
  `(let ((*general* (make-instance 'general-expressions)))
     ,@body))

(defmacro check-point-context (&body body)
  `(let ((.old-variables. (ge-variables *general*))
	 (.old-context. (ge-context *general*)))
     (unwind-protect (progn ,@body)
       (setf .old-variables. (ge-variables *general*))
       (setf .old-context. (ge-context *general*)))))

(defmethod coerce ((exp list) (domain general-expressions))
  (flet ((coerce-obj (x)
	   (coerce x domain)))
    (cond ((eql (first exp) '+)
	   (make-ge-plus domain
			 (mapcar #'coerce-obj (rest exp))))
	  ((eql (first exp) '*)
	   (make-ge-times domain
			 (mapcar #'coerce-obj (rest exp))))
	  ((eql (first exp) '-)
	   (if (null (rest (rest exp)))
	       (make-ge-times domain (list -1 (coerce-obj (second exp))))
	       (make-ge-plus domain
		 (list (coerce-obj (second exp))
		       (make-ge-times domain
			 (cons (make-element domain -1)
			       (mapcar #'coerce-obj (rest (rest exp)))))))))
	  ((eql (first exp) '/)
	   (make-ge-times domain
	     (list (coerce-obj (second exp))
		   (make-ge-expt domain
		     (make-ge-times domain
		       (mapcar #'coerce-obj (rest (rest exp))))
		     (make-element domain -1)))))
	  (t (error "Don't know how to coerce ~S into ~S"
		    exp domain)))))			     
			     
(defmethod get-variable-property ((domain domain) (var ge-variable) key)
  (loop for var-prop in (ge-context domain)
	do (when (eql (first var-prop) var)
	     (return (%getf (rest var-prop) key)))
	finally (progn 
		  (push (list var) (ge-context domain))
		  (return nil))))

(defmethod set-variable-property (domain (var ge-variable) key value)
  (loop for var-prop in (ge-context domain)
	do (when (eql (first var-prop) var)
	     (setf (%getf (rest var-prop) key) value)
	     (return value))	
	finally (progn 
		  (push (list var key value) (ge-context domain))
		  (return value))))

(defsetf get-variable-property set-variable-property)

(defun parenthesized-display (expr stream)
  (princ "(" stream)
  (print-object expr stream)
  (princ ")" stream))

(defun safe-display (expr stream)
  (if (or (and (number? expr) (not (typep expr 'complex-number)))
	  (and (typep expr 'complex-number)
	       (0? (realpart expr)))
	  (ge-variable? expr)
	  (ge-expt? expr))
      (print-object expr stream)      
      (parenthesized-display expr stream)))

;; Display a list of objects, paying attention to *print-length*.  No
;; surrounding delimiters.   This is a method so that we can define
;; similar functions for sets of objects embedded in arrays.
(defmethod display-list
    ((objects list) &optional (stream *standard-output*))
  (when objects
    (let ((cnt (or *print-length* -1)))
      (declare (fixnum cnt))
      (print-object (first objects) stream)
      (cl:decf cnt)
      (loop for var in (rest objects)
	    do (princ ", " stream)
	       (when (cl:zerop cnt)
		 (princ "..." stream)
		 (return))
	       (print-object var stream)
	       (cl:decf cnt)))))


;; Ordering functions for general expressions

;; Some operators may choose to ignore various parameters here.

(defun ge-lequal (x y)
  (loop
    (when (and (null x) (null y))
      (return-from ge-lequal t))
    (when (or (null x) (null y)
	      (not (ge-equal (first x) (first y))))
      (return-from ge-lequal nil))
    (pop x) (pop y)))

(defun ge-lgreat (x y)
  (loop 
    (cond ((null x)
	   (return nil))
	  ((null y)
	   (return t))
	  ((ge-equal (first x) (first y)))
	  ((ge-great (first x) (first y))
	   (return t))
	  (t (return nil)))
    (pop x) (pop y)))


;; x is assumed to be a variable


(defmethod real? ((x number))
  (not (cl:complexp x)))

(defmethod real? ((x bigfloat))
  t)

(defmethod real? ((x numeric))
  (not (typep x 'complex-number)))

(defmethod minus? ((x t))
  nil)

;; For compatibility with Common Lisp
(defun minusp (x) (minus? x))
(defun plusp (x) (plus? x))
(defun zerop (x) (0? x))

(defun ge-minus? (x)
  (cond ((and (number? x) (real? x)) (minus? x))
	((ge-times? x)
	 (let ((lead-term (first (terms-of x))))
	   (and (and (number? lead-term)
		     (real? lead-term)
		     (minus? lead-term)))))
	(t nil)))

;; This works by converting the sum into a list of dotted pairs.  The
;; first element of the list is a number, while the second is a list
;; of product terms.  This makes combining new elements quite easy.
;; After the combination, everything is converted back to the standard
;; representation. 

(defmacro merge-terms-in-sum (terms &body body)
  `(let ((,terms (list nil)))
     (labels ((add-term (base order) 
		(loop with terms = ,terms do
		  (cond ((or (null (rest terms))
			     (ge-lgreat base (rest (second terms))))
			 (push (cons order base) (rest terms))
			 (return t))
			((ge-lequal base (rest (second terms)))
			 (setf (first (second terms))
			       (+ (first (second terms)) order))
			 (when (0? (first (second terms)))
			   (setf (rest terms) (rest (rest terms))))
			 (return t)))
		  (pop terms))))
       ,@body)))


(defmethod make-ge-plus ((domain general-expressions) terms)
  (make-instance 'ge-plus :domain domain :terms terms))

(defmethod print-object ((sum ge-plus) stream)
  (let ((terms (terms-of sum)))
    (print-object (first terms) stream)
    (loop for x in (rest terms)
	  do (cond ((and (number? x) (real? x))
		    (if (plus? x)
			(format stream " + ~S" x)
			(format stream " - ~S" (minus x))))
		   ((ge-minus? x)
		    (princ " - " stream)
		    (safe-display 
		     (simp-times-terms (domain-of sum) (list -1 x))
		     stream))
		   (t (princ " + " stream)
		      (print-object x stream))))))

(defmethod simplify ((x ge-plus))
  (simp-plus-terms (domain-of x) (terms-of x)))
  
(defun simp-plus-terms (domain old-terms)
  (merge-terms-in-sum terms
    (let ((const 0))
      (labels ((loop-over-terms (terms)
		 (loop for term in terms
		       do (setq term (simplify term))
			  (cond ((number? term) 
				 (setq const (+ const term)))
				((ge-plus? term)
				 (loop-over-terms (terms-of term)))
				((ge-times? term)
				 (setq term (terms-of term))
				 (cond ((number? (first term))
					(add-term (rest term) (first term)))
				       (t (add-term term 1))))
				(t (add-term (list term) 1))))))
	(loop-over-terms old-terms)
	(setq terms (loop for (c . term-l) in (rest terms)
			  collect
			  (if (or (eql c 1) (eql c 1.0))
			      (if (null (rest term-l))
				  (first term-l)
				  (simplify
				   (make-ge-times domain term-l)))
			      (simplify
			       (make-ge-times domain (cons c term-l)))))) 
	(cond ((not (0? const))
	       (if (null terms) const
		   (make-ge-plus domain (cons const terms))))
	      ((null terms)
	       (make-element domain 0))
	      ((null (rest terms))
	       (first terms))
	      (t (make-ge-plus domain terms)))))))

(defmethod ge-equal ((x ge-plus) (y ge-plus))
  (ge-lequal (terms-of x) (terms-of y)))

(defmethod ge-great ((x ge-plus) (y ge-plus))
  (ge-lgreat (terms-of x) (terms-of y)))


(defmethod make-ge-times ((domain general-expressions) terms)
  (make-instance 'ge-times :domain domain :terms terms))

(defmethod print-object ((x ge-times) stream)
  (let ((terms (terms-of x)))
    (safe-display (first terms) stream)
    (loop for x in (rest terms)
	  do (princ " " stream)
	     (safe-display x stream))))

(defmethod simplify ((x ge-times)) 
  (simp-times-terms (domain-of x) (terms-of x)))

(defun simp-times-terms (domain old-terms)
  (merge-terms-in-sum terms 
    (let ((const 1))
      (labels ((loop-over-terms (terms) 
		 (loop for term in terms do
		   (setq term (simplify term))
		   (cond ((number? term)
			  (when (0? term)
			    (return-from simp-times-terms
			      (make-element domain 0)))
			  (setq const (* const term)))
			 ((ge-times? term)
			  (loop-over-terms (terms-of term)))
			 ((ge-expt? term)
			  (let ((exp (exponent-of term))
				(base (base-of term)))
			    (cond ((number? (exponent-of term))
				   (add-term (list base) exp))
				  (t (add-term (list base)
					       (make-element domain 1))))))
			 (t (add-term (list term) 1))))))
	(loop-over-terms old-terms)
	(setq terms (loop for (exp base) in (rest terms)
			  collect
			  (if (1? exp) base
			      (make-ge-expt domain base exp))))
	(cond ((not (1? const))
	       (if (null terms)
		   const
		   (make-ge-times domain (cons const terms))))
	      ((null terms)
	       (make-element domain 1))
	      ((null (rest terms))
	       (first terms))
	      (t (make-ge-times domain terms)))))))

(defmethod ge-equal ((x ge-times) (y ge-times))
  (ge-lequal (terms-of x) (terms-of y)))

(defmethod ge-great ((x ge-times) (y ge-times))
  (ge-lgreat (terms-of x) (terms-of y)))


(defmethod make-ge-expt ((domain general-expressions) base exp)
  (make-instance 'ge-expt :domain domain :base base :exp exp))

(defmethod simplify ((x ge-expt))
  (let ((exp (simplify (exponent-of x)))
	(base (base-of x)))
    (cond ((0? exp) 1)
	  ((1? exp) (simplify base))
	  ((and (number? (setq base (simplify base)))
		(number? exp))
	   (expt base exp))
	  ((ge-expt? base)
	   (simplify 
	    (make-ge-expt (domain-of x) (base-of base)
			  (* (exponent-of base) exp))))
	  (t (make-ge-expt (domain-of x) (simplify (base-of x)) exp)))))

(defmethod print-object ((expr ge-expt) stream)
  (safe-display (base-of expr) stream)
  (princ "^" stream)
  (safe-display (exponent-of expr) stream))

(defmethod ge-equal ((x ge-expt) (y ge-expt))
  (and (ge-equal (base-of x) (base-of y))
       (ge-equal (exponent-of x) (exponent-of y))))

(defmethod ge-great ((x ge-expt) (y ge-expt))
  (cond ((ge-great (base-of x) (base-of y))
	 t)
	((ge-equal (base-of x) (base-of y))
	 (ge-great (exponent-of x) (exponent-of y)))
	(t nil)))

(defmethod ge-equal ((x ge-application) (y ge-application))
  (and (eql (funct-of x) (funct-of y))
       (ge-lequal (args-of x) (args-of y))))

(defmethod ge-equal ((x ge-function) (y ge-function))
  (eql x y))

;; Derivatives

(defmethod ge-deriv (exp var)
  (error "Don't know how to take the derivative of ~S wrt ~S"
	 exp var))

(defmethod deriv ((exp number) &rest vars)
  (if (null vars)
      (make-element *general* exp)
      (make-element *general* 0)))

(defmethod deriv ((exp numeric) &rest vars)
  (if (null vars) exp
      (make-element (domain-of exp) 0)))

(defmethod deriv ((exp (or symbol general-expression)) &rest vars)
  (setq exp (coerce exp *general*))
  (loop for v in vars
	do (setq exp (ge-deriv exp (coerce v *general*))))
  exp)

(defmethod deriv ((fun ge-function) &rest args)
  (loop for i in args
	with nargs = (nargs-of fun)
	do (when (or (minusp i) (not (< i nargs)))
	     (error "Illegal derivative of ~S in position ~D" fun i)))
  (cond ((null args)
	 fun)
	((getf fun 'deriv)
	 (apply #'deriv (nth (first args) (getf fun 'deriv))
		(rest args)))
	(t (make-function-deriv fun (copy-list args)))))

(defmethod ge-deriv ((exp general-expression) (var symbol))
  (ge-deriv exp (coerce var (domain-of exp))))

(defmethod-sd ge-deriv ((exp numeric) (var ge-atom))
  (make-element domain 0))

(defmethod-sd ge-deriv ((exp ge-atom) (var ge-atom))
  (cond ((ge-equal exp var) (make-element domain 1))
        #+ignore
	((depends-on? exp var)
	 (make-ge-deriv domain exp `((,var 1))))
	(t (make-element domain 0))))

(defmethod-sd ge-deriv ((exp ge-plus) (var ge-atom))
  (simplify
   (make-ge-plus domain (loop for x in (terms-of exp)
			      collect (ge-deriv x var)))))

(defmethod-sd ge-deriv ((exp ge-times) (var ge-atom))
  (let ((terms (terms-of exp)))
    (simplify
     (make-ge-plus domain
       (loop for x in terms
	     collect
	     (simplify
	       (make-ge-times domain
		 (cons (ge-deriv x var) (remove x terms)))))))))

(defmethod-sd ge-deriv ((exp ge-expt) (var ge-atom))
  (let ((base (base-of exp))
	(power (exponent-of exp)))
    (cond ((depends-on? power var)
	   (error "Not yet implemented"))
	  ((and (number? power) (= power 2))
	   (* 2 base (ge-deriv base var)))
	  (t (* power (expt base (- power 1)))))))

(defmethod-sd ge-deriv ((exp ge-application) (var ge-atom))
  (let* ((args (args-of exp))
         (fun (funct-of exp))
         (dargs (loop for arg in args
                     collect (ge-deriv arg var)))
         (derivs (getf fun 'deriv))
         ans)
    (when (null derivs)
      (setq derivs (loop for i below (nargs-of fun)
                         collect (make-function-deriv fun i)))
      (setf (getf fun 'deriv) derivs))
    (loop for darg in dargs
          for deriv in derivs
          do  (unless (0? darg)
               (push (if (1? darg)
                       (apply deriv args)
                       (* darg (apply deriv args)))
                     ans)))
    (cond ((null ans)
	   (zero domain))
	  ((rest ans) (make-ge-plus domain ans))
	  (t (first ans)))))

(defmethod make-ge-eqn= ((domain general-expressions) lhs rhs)
  (make-instance 'ge-eqn= :domain domain :rhs rhs :lhs lhs))

(defmethod print-object ((eqn ge-eqn=) stream)
  (print-object (lhs-of eqn) stream)
  (princ " = " stream)
  (print-object (rhs-of eqn) stream))

(defmethod eqn= (lhs rhs)
  (make-ge-eqn= *general*
		(simplify (coerce lhs *general*))
		(simplify (coerce rhs *general*))))

(defmethod simplify ((eqn ge-eqn=))
  (make-ge-eqn= (domain-of eqn)
		(simplify (lhs-of eqn))
		(simplify (rhs-of eqn))))

(defmethod-sd ge-deriv ((eqn ge-eqn=) (var ge-atom))
  (make-ge-eqn= domain
		(ge-deriv (lhs-of eqn) var)
		(ge-deriv (rhs-of eqn) var)))

(defmethod make-ge-eqn> ((domain general-expressions) lhs rhs)
  (make-instance 'ge-eqn> :domain domain :rhs rhs :lhs lhs))

(defmethod print-object ((eqn ge-eqn>) stream)
  (print-object (lhs-of eqn) stream)
  (princ " > " stream)
  (print-object (rhs-of eqn) stream))

(defmethod eqn> (lhs rhs)
  (make-ge-eqn> *general*
		(simplify (coerce lhs *general*))
		(simplify (coerce rhs *general*))))

(defmethod simplify ((eqn ge-eqn>))
  (make-ge-eqn> (domain-of eqn)
		(simplify (lhs-of eqn))
		(simplify (rhs-of eqn))))

(defmethod-sd ge-deriv ((eqn ge-eqn>) (var ge-atom))
  (make-ge-eqn> domain
		(ge-deriv (lhs-of eqn) var)
		(ge-deriv (rhs-of eqn) var)))

(defmethod make-ge-eqn>= ((domain general-expressions) lhs rhs)
  (make-instance 'ge-eqn>= :domain domain :rhs rhs :lhs lhs))

(defmethod print-object ((eqn ge-eqn>=) stream)
  (print-object (lhs-of eqn) stream)
  (princ " >= " stream)
  (print-object (rhs-of eqn) stream))

(defmethod eqn>= (lhs rhs)
  (make-ge-eqn>= *general*
		 (simplify (coerce lhs *general*))
		 (simplify (coerce rhs *general*))))

(defmethod simplify ((eqn ge-eqn>=))
  (make-ge-eqn>= (domain-of eqn)
		 (simplify (lhs-of eqn))
		 (simplify (rhs-of eqn))))

(defmethod-sd ge-deriv ((eqn ge-eqn>=) (var ge-atom))
  (make-ge-eqn>= domain
		 (ge-deriv (lhs-of eqn) var)
		 (ge-deriv (rhs-of eqn) var)))

(defmethod eqn-complement ((ineq ge-eqn>))
  (eqn>= (rhs-of ineq) (lhs-of ineq)))

(defmethod eqn-complement ((ineq ge-eqn>=))
  (eqn> (rhs-of ineq) (lhs-of ineq)))


;; Scalar versus scalar is covered by numbers.lisp
;; These are just supposed to be the other cases.  

(defmacro define-ge2-standard-methods (op)
  `(progn
     (defmethod ,op ((x (or number symbol)) (y symbol))
       (,op (coerce x *general*) (coerce y *general*)))

     (defmethod ,op ((x symbol) (y number))
       (,op (coerce x *general*) (coerce y *general*)))

     (defmethod ,op ((x general-expression) (y symbol))
       (,op x (coerce y (domain-of x))))

     (defmethod ,op ((x symbol) (y general-expression))
       (,op (coerce x (domain-of y)) y))))

;; When a numeric from a different domain is added to a general
;; expression, the code in morphisms.lisp will coerce the numeric to
;; the *general* (a non-strict-domain) and then come back here.

(define-ge2-standard-methods plus)

(defmethod-sd plus ((x ge-or-numeric) (y ge-or-numeric))
  (simplify (make-ge-plus domain (list x y))))

(defmethod-sd plus ((eq1 ge-eqn=) (eq2 ge-eqn=))
  (make-ge-eqn= domain
		(+ (lhs-of eq1) (lhs-of eq2))
		(+ (rhs-of eq1) (rhs-of eq2))))

(defmethod-sd plus ((eq1 ge-eqn=) (exp ge-or-numeric))
  (make-ge-eqn= domain (+ (lhs-of eq1) exp) (+ (rhs-of eq1) exp)))

(defmethod-sd plus ((exp ge-or-numeric) (eq1 ge-eqn=))
  (make-ge-eqn= domain (+ (lhs-of eq1) exp) (+ (rhs-of eq1) exp)))


(define-ge2-standard-methods difference)

(defmethod-sd difference ((x ge-or-numeric) (y ge-or-numeric))
  (simplify (make-ge-plus domain
	      (list x (make-ge-times domain
				     (list (make-element domain -1) y))))))

(defmethod-sd difference ((eq1 ge-eqn=) (eq2 ge-eqn=))
  (make-ge-eqn= domain
		(- (lhs-of eq1) (lhs-of eq2))
		(- (rhs-of eq1) (rhs-of eq2))))

(defmethod-sd difference ((eq1 ge-eqn=) (exp ge-or-numeric))
  (make-ge-eqn= domain (- (lhs-of eq1) exp) (- (rhs-of eq1) exp)))

(defmethod-sd difference ((exp ge-or-numeric) (eq1 ge-eqn=))
  (make-ge-eqn= domain (- exp (lhs-of eq1)) (- exp (rhs-of eq1))))


(defmethod minus ((x symbol))
  (- (coerce x *general*)))

(defmethod minus ((x general-expression))
  (let ((domain (domain-of x)))
    (simplify
     (make-ge-times domain (list (make-element domain -1) x)))))

(defmethod minus ((eq1 ge-eqn=))
  (make-ge-eqn= (domain-of eq1) (- (lhs-of eq1)) (- (rhs-of eq1))))
							  

(define-ge2-standard-methods times)

(defmethod-sd times ((x ge-or-numeric) (y ge-or-numeric))
  (simplify (make-ge-times domain (list x y))))

(defmethod-sd times ((eq1 ge-eqn=) (eq2 ge-eqn=))
  (error "Can't multiply two equations"))

(defmethod-sd times ((eq1 ge-eqn=) (exp ge-or-numeric))
  (make-ge-eqn= domain (* (lhs-of eq1) exp) (* (rhs-of eq1) exp)))

(defmethod-sd times ((exp ge-or-numeric) (eq1 ge-eqn=))
  (make-ge-eqn= domain (* (lhs-of eq1) exp) (* (rhs-of eq1) exp)))


(define-ge2-standard-methods quotient)

(defmethod-sd quotient ((x ge-or-numeric) (y ge-or-numeric))
  (simplify (make-ge-times domain
	      (list x (make-ge-expt domain y (make-element domain -1))))))

(defmethod-sd quotient ((eq1 ge-eqn=) (eq2 ge-eqn=))
  (error "Can't divide two equations"))

(defmethod-sd quotient ((eq1 ge-eqn=) (exp ge-or-numeric))
  (make-ge-eqn= domain (/ (lhs-of eq1) exp) (/ (rhs-of eq1) exp)))

(defmethod-sd quotient ((exp domain-element) (eq1 ge-eqn=))
  (error "Can't divide by an equation"))


(defmethod recip ((x symbol))
  (recip (coerce x *general*)))

(defmethod recip ((x general-expression))
  (let ((domain (domain-of x)))
    (simplify (make-ge-expt domain x (make-element domain -1)))))

(defmethod recip ((eq1 ge-eqn=))
  (make-ge-eqn= (domain-of eq1) (/ (lhs-of eq1)) (/ (rhs-of eq1))))


(define-ge2-standard-methods expt)

(defmethod-sd expt ((x general-expression) (y ge-or-numeric))
  (simplify (make-ge-expt domain x y)))

(defmethod-sd expt ((x numeric) (y general-expression))
  (simplify (make-ge-expt domain x y)))

(defmethod-sd expt ((eq1 ge-eqn=) (eq2 ge-eqn=))
  (error "Can't exponentiate two equations"))

(defmethod-sd expt ((eq1 ge-eqn=) (exp ge-or-numeric))
  (make-ge-eqn= domain (expt (lhs-of eq1) exp) (expt (rhs-of eq1) exp)))

(defmethod-sd expt ((exp ge-or-numeric) (eq1 ge-eqn=))
  (error "Can't put an equation in an exponent"))

(defmethod make-union ((var symbol) (set set) (expr general-expression)
		       &rest rest-exprs)
  (apply #'make-union (coerce var (domain-of expr)) set expr rest-exprs))

(defun make-universal-quantified-set (domain bound-vars expressions)
  (make-instance 'universal-quantified-set
		 :domain domain
		 :bound-vars bound-vars
		 :expressions expressions
		 :print-function 'uq-set-print-object))

(defun uq-set-print-object (set stream)
  (let ((bound-vars (bound-vars-of set)))
    (format stream "{ForAll ~S in ~S~:{, ~S in ~S~} . ~S~{, ~S~}}" 
	    (first (first bound-vars)) (second (first bound-vars))
	    (rest bound-vars)
	    (first (exprs-of set)) (rest (exprs-of set)))))

;; This used to be just variables, but it makes more sense for one to be
;; able to use quantifiers over any atomic object.
(defmethod make-union (var set (expr general-expression)
		       &rest rest-exprs)
  (let ((domain (domain-of expr)))
    ;;; Make sure that the union is sensible.
    (dolist (x rest-exprs)
      (if (not (eq domain (domain-of x)))
	  (error "Can't union incompatible domains.")))
    (simplify
      (make-universal-quantified-set domain (list (list var set))
				     (cons expr rest-exprs)))))

;; The following function examines each element of EXPRS to see if
;; they contain any bound variables.  If so, its bound variables are
;; merged with BOUND-VARS.  Two values are returned the list of bound
;; variables and expressions.  The variable TYPE indicates the type of
;; quantification.
(defun merge-bound-vars (type bound-vars exprs)
  (let ((new-exprs nil))
    (flet ((merge-new-bv (var set)
	     (loop for (v) in bound-vars
		   do (when (ge-equal var v)
			(error "Don't know how deal with scoping"))
		   finally (push (list var set) bound-vars))))
      (loop for expr in exprs
	    do (setq expr (simplify expr))
	       (cond ((not (typep expr type))
		      (push expr new-exprs))
		     (t (loop for (var set) in (bound-vars-of expr)
			      do (merge-new-bv var set))
			(setq new-exprs (append new-exprs (exprs-of expr))))))
      (values bound-vars new-exprs))))       
  
(defmethod simplify ((set universal-quantified-set))
  (multiple-value-bind (bv exprs) (merge-bound-vars 'universal-quantified-set
						    (bound-vars-of set)
						    (exprs-of set))

    (make-universal-quantified-set (domain-of set) bv exprs)))

;; Variable dependencies and DEPENDS-ON? 

(defmethod declare-dependencies ((var ge-variable) &rest vars)
  (let ((depends (get-variable-property (domain-of var) var :dependencies))
	(domain (domain-of var)))
    (loop for v in vars
	  do (setq v (coerce v domain))
	     (unless (member v depends :test #'ge-equal)
	       (push v depends)))
    (setf (get-variable-property (domain-of var) var :dependencies) depends)))

;; ge-same-var returns true if two variables have the same symbol
(defun ge-same-var (v1 v2)
  (if (and (ge-variable? v1) (ge-variable? v2))
      (equal (string-of v1) (string-of v2))
      (eql v1 v2)))

;; The following method allows one to check if any of several
;; expressions depend on a variable easily.
(defmethod depends-on? ((exp list) &rest vars)
  (loop for arg in exp
	do (when (apply #'depends-on? arg vars)
	     (return t))
	finally (return nil)))

(defmethod depends-on? ((exp (or number numeric)) &rest vars)
  (declare (ignore vars))
  nil)

(defmethod depends-on? ((exp ge-variable) &rest vars)
  (or (member exp vars :test 'ge-same-var)
      (loop for var in (get-variable-property (domain-of exp)
			   exp :dependencies)
	    do (when (member var vars :test #'ge-equal)
		 (return t))
	    finally (return nil))))

(defmethod depends-on? ((exp ge-function) &rest vars)
  (loop for var in vars
	do (when (and (typep var 'ge-function)
		      (eql (name-of exp) (name-of var)))
	     (return t))
	finally (return nil)))

(defmethod depends-on? ((exp ge-application) &rest vars)
  (or (apply #'depends-on? (funct-of exp) vars)
      (apply #'depends-on? (args-of exp) vars)))		     

(defmethod depends-on? ((exp ge-plus) &rest vars)
  (apply #'depends-on? (terms-of exp) vars))

(defmethod depends-on? ((exp ge-times) &rest vars)
  (apply #'depends-on? (terms-of exp) vars))

(defmethod depends-on? ((exp ge-expt) &rest vars)
  (or (apply #'depends-on? (base-of exp) vars)
      (apply #'depends-on? (exponent-of exp) vars)))

(defmethod depends-on? ((equation ge-equation) &rest vars)
  (or (apply #'depends-on? (lhs-of equation) vars)
      (apply #'depends-on? (rhs-of equation) vars)))

;; Different kernels

(defmethod different-kernels ((exp numeric) (kernels list))
  kernels)

;; If we don't know anything about the object, then its a kernel.
(defmethod different-kernels (exp (kernels list))
  (if  (member exp kernels :test #'ge-equal)
       kernels
       (cons exp kernels)))

(defmethod different-kernels ((exp ge-plus) (kernels list))
  (loop for term in (terms-of exp)
	do (setq kernels (different-kernels term kernels)))
  kernels)

(defmethod different-kernels ((exp ge-times) (kernels list))
  (loop for term in (terms-of exp)
	do (setq kernels (different-kernels term kernels)))
  kernels)

(defmethod different-kernels ((exp ge-expt) (kernels list))
  (if (or (cl:integerp (exponent-of exp))
	  (typep (exponent-of exp) 'rational-integer))
      (different-kernels (base-of exp) kernels)
      (call-next-method)))

(defmethod different-kernels ((exp ge-equation) (kernels list))
  (different-kernels (lhs-of exp)
		     (different-kernels (rhs-of exp) kernels)))

(defmethod different-kernels ((exp list) (kernels list))
  (loop for e in exp
	do (setq kernels (different-kernels e kernels)))
  kernels)


(defmethod substitute (value var expr &rest ignore)
  (declare (ignore value var ignore))
  expr)

(defmethod substitute (value (var symbol) expr &rest ignore)
  (declare (ignore ignore))
  (substitute value (coerce var (domain-of expr)) expr))

(defmethod substitute (value (var ge-variable) (expr (or number numeric))
     &rest ignore)
  (declare (ignore value ignore))
  expr)

(defmethod substitute (value (var ge-variable) (expr ge-variable) &rest ignore)
  (declare (ignore ignore)) 
  (if (eql var expr) (coerce value (domain-of expr)) expr))

(defmethod substitute
    (value (var ge-function) (expr ge-function) &rest ignore)
  (declare (ignore ignore))
  (if (eql var expr) value expr))

(defmethod substitute (value var (expr ge-plus) &rest ignore)
  (declare (ignore ignore))
  (apply #'%plus (mapcar #'(lambda (q) (substitute value var q))
			 (terms-of expr))))

(defmethod substitute (value var (expr ge-times) &rest ignore)
  (declare (ignore ignore))
  (apply #'%times (mapcar #'(lambda (q) (substitute value var q))
			 (terms-of expr))))

(defmethod substitute (value var (expr ge-expt) &rest ignore)
  (declare (ignore ignore))
  (expt (substitute value var (base-of expr))
	(substitute value var (exponent-of expr))))

(defmethod substitute (value (var ge-variable) (expr ge-application)
			     &rest ignore)
  (declare (ignore ignore))
  (apply (funct-of expr)
	  (mapcar #'(lambda (q) (substitute value var q))
		  (args-of expr))))

;; FIXTHIS Bummed to make finite element work......
(defvar *fem-kludge* nil)

(defmethod substitute (value (var ge-function) (expr ge-application)
			     &rest ignore)
  (declare (ignore ignore))
  (let ((name (name-of var))
	(funct (funct-of expr)))
    (if *fem-kludge*
	(cond ((string= name (name-of funct))
	       (if (typep funct 'ge-function-deriv)
		   (apply #'deriv value (derivs-of funct))
		   value))
	      (t (funct-of expr)))
	(apply (cond ((string= name (name-of funct))
		      (if (typep funct 'ge-function-deriv)
			  (apply #'deriv value (derivs-of funct))
			  value))
		     (t (funct-of expr)))
	       (mapcar #'(lambda (q) (substitute value var q))
		       (args-of expr))))))

(defmethod substitute (value var (expr ge-equation) &rest ignore)
  (declare (ignore ignore))
  (make-instance (class-of expr)
		 :domain (domain-of expr)
		 :lhs (substitute value var (lhs-of expr))
		 :rhs (substitute value var (rhs-of expr))))


(defmethod expand ((exp t))
  exp)

(defmethod expand ((exp ge-times))
  (let ((forms (expand-product exp)))
    (if (null (rest forms))
	(first forms)
	(simp-plus-terms (domain-of exp) forms))))

(defun expand-product (exp)
  (loop for list in (expand-product1 (terms-of exp))
	collect (simp-times-terms (domain-of exp) list)))

(defun expand-product1 (terms)
  (flet ((expand-plus (term rest)
	   (loop with expanded-terms = (expand-product1 rest)
		 for x in (terms-of term)
		 append (loop for et in expanded-terms
			      collect (cons x et))))
	 (expand-other (term rest)
	   (loop for et in (expand-product1 rest)
		 collect (cons term et))))
    (cond ((null terms)
	   (list ()))
	  ((ge-plus? (first terms))
	   (expand-plus (first terms) (rest terms)))
	  ((ge-expt? (first terms))
	   (let ((temp (expand (first terms))))
		(cond ((ge-plus? temp)
		       (expand-plus temp (rest terms)))
		      (t (expand-other temp (rest terms))))))
	  (t (expand-other (first terms) (rest terms))))))
	       
(defmethod expand ((exp ge-plus))
  (let ((expanded-terms nil)
	(terms (terms-of exp))
	term)
    (loop while terms do
      (setq term (expand (pop terms)))
      (if (ge-plus? term)
	  (setq terms (append terms (terms-of term)))
	  (push term expanded-terms)))
    (simp-plus-terms (domain-of exp) expanded-terms)))

(defmethod expand ((exp ge-expt))
  (let ((base (base-of exp))
	(exponent (exponent-of exp)))
    (cond ((and (ge-plus? base)
		(typep exponent 'rational-integer))
	   (simp-plus-terms (domain-of exp)
			     (expand-binomial-form (terms-of base)
						   (integer-value exponent))))
	  ((ge-times? base)
	   (expand (simp-times-terms (domain-of exp)
		     (loop for term in (terms-of base)
			   collect (expand (expt term exponent))))))
	  (t exp))))

(defun expand-binomial-form (terms n)
  (cond ((null (rest terms))
	 (list (expt (first terms) n)))
	(t (let ((sum ())
		 (a (first terms))
		 (b (rest terms)))
	     (loop for i below (1+ n)
		   for coef = (combinations n i)
		   do (loop for term in (expand-binomial-form b i)
			    do (push (* coef (expt a (- n i)) term) sum)))
	     sum))))
