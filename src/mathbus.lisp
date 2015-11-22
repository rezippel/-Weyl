;;; -*- Mode:Lisp; Package:CL-User; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;		    Mathbus General Representation Translator
;;; ===========================================================================
;;; (c) Copyright 1996, 1997 Cornell University

;;; $Header: terms.lisp,v 1.1 1994/01/24 22:11:53 rz Exp $

(in-package "WEYLI")

;; Conversions from Weyl's general representation to the Mathbus are
;; done using the convert-to-mb operator.

(defmethod convert-to-mb ((int integer))
  (mathbus::mb-integer int))

(defmethod convert-to-mb ((int rational-integer))
   (mathbus:mb-integer (integer-value int)))

(defmethod convert-to-mb ((var ge-variable))
  (let ((strId (mathbus:declare-local-stringId (string-of var))))
    (mathbus:mbnode 'AlgVariable strId)))

(defmethod convert-to-mb ((expr ge-plus))
  (let* ((terms (terms-of expr))
	 (node (mathbus:make-mbnode mathbus:MBS_Plus (length terms))))
    (loop for i below (length terms)
	  for term in terms
	  do (setf (mathbus:mbnode-subterm node (1+ i)) (convert-to-mb term)))
     node))

(defmethod convert-to-mb ((expr ge-times))
  (let* ((terms (terms-of expr))
	 (node (mathbus:make-mbnode mathbus:MBS_Times (length terms))))
    (loop for i below (length terms)
	  for term in terms
	  do (setf (mathbus:mbnode-subterm node (1+ i)) (convert-to-mb term)))
     node))

(defmethod convert-to-mb ((expr ge-expt))
  (mathbus:mbnode 'Exponentiate 
	  (convert-to-mb (base-of expr))
	  (convert-to-mb (exponent-of expr))))

(defvar *weyl-func-table* (make-hash-table))

(defmacro defmathbus-function (weyl-name nargs mathbus-name)
  `(let ((mb-lab (mathbus::numeric-label ,mathbus-name)))
    (setf (getf (make-function nil ',weyl-name ,nargs) 'mathbus-label)
          mb-lab)
    (setf (gethash mb-lab *weyl-func-table*) ',weyl-name)
    ,mathbus-name))

(defmathbus-function abs 1 "Absolute")
(defmathbus-function realpart 1 "RealPart")
(defmathbus-function imagpart 1 "ImaginaryPart")
(defmathbus-function log 1 "Logarithm")
(defmathbus-function sin 1 "Sine")
(defmathbus-function cos 1 "Cosine")
(defmathbus-function tan 1 "Tangent")
(defmathbus-function asin 1 "ArcSine")
(defmathbus-function acos 1 "ArcCosine")
(defmathbus-function atan 1 "ArcTangent")
(defmathbus-function sinh 1 "HypSine")
(defmathbus-function cosh 1 "HypCosine")
(defmathbus-function tanh 1 "HypTangent")
(defmathbus-function asinh 1 "HypArcSine")
(defmathbus-function acosh 1 "HypArcCosine")
(defmathbus-function atanh 1 "HypArcTangent")

(defmethod convert-to-mb ((expr ge-application))
  (let* ((funct (funct-of expr))
	 (args (args-of expr))
	 (node (mathbus:make-mbnode mathbus:MBS_Application
				    (1+ (length args))))
	 fun temp)
    (setq fun (cond ((setq temp (getf funct 'mathbus-label))
		     (mathbus:mbnode 'AlgFuncCode temp))
		    ((ge-function? funct)
		     (setq temp
			   (mathbus:declare-local-stringId (name-of funct)))
		     (setf (getf funct 'mathbus-label) temp)
		     (mathbus:mbnode 'AlgFuncCode temp))
		    (t (error "Don't know how to deal with this"))))
    
    (when (ge-function-deriv? funct)
      (let* ((derivs (derivs-of funct))
	     (lim (length derivs))
	     deriv)
	(setq deriv (mathbus:make-mbnode mathbus:MBS_AlgDerivative
                                         (1+ lim)))
	(loop for i upfrom 2 below (+ lim 2)
	      for d in derivs
	      do (setf (mathbus:mbnode-subterm deriv i)
		       (1+ d)))
	(setf (mathbus:mbnode-subterm deriv lim) fun)
	(setq fun deriv)))
				       
    (setf (mathbus:mbnode-subterm node 1) fun)

    (loop for i upfrom 2
	  for term in args
	  do (setf (mathbus:mbnode-subterm node i) (convert-to-mb term)))
    node))

(defmethod convert-to-mb ((eqn ge-eqn=))
  (mathbus:mbnode 'EquationEqual
		  (convert-to-mb (lhs-of eqn)) 
		  (convert-to-mb (rhs-of eqn))))

(defmethod convert-to-mb ((eqn ge-eqn>))
  (mathbus:mbnode 'EquationGreat
		  (convert-to-mb (lhs-of eqn)) 
		  (convert-to-mb (rhs-of eqn))))

(defmethod convert-to-mb ((eqn ge-eqn>=))
  (mathbus:mbnode 'EquationGreatEqual
		  (convert-to-mb (lhs-of eqn)) 
		  (convert-to-mb (rhs-of eqn))))

(defun convert-to-weyl (node)
  (let ((label (mathbus:mbnode-label node)))
    (cond ((eql label mathbus:MBS_LongInteger)
	   (coerce (mathbus::integer-value node) *general*))
	  ((eql label mathbus:MBS_AlgVariable)
	   (coerce (intern (mathbus::registry-lookup-identifier "StringId"
                                (mathbus:mbnode-subterm node 1))
			   'weyli)
		   *general*))
	  ((eql label mathbus:MBS_Plus)
	   (let ((w-terms nil))
	     (mathbus::loop-over-subterms node (i type)
               (push (convert-to-weyl (mathbus:mbnode-subterm node i))
		     w-terms))
	     (make-ge-plus *general* (nreverse w-terms))))
	  ((eql label mathbus:MBS_Minus)
	   (* -1 (convert-to-weyl (mathbus:mbnode-subterm node 1))))
	  ((eql label mathbus:MBS_Times)
	   (let ((w-terms nil))
	     (mathbus::loop-over-subterms node (i type)
               (push (convert-to-weyl (mathbus:mbnode-subterm node i))
		     w-terms))
	     (make-ge-times *general* (nreverse w-terms))))
	  ((eql label mathbus:MBS_Exponentiate)
	   (expt (convert-to-weyl (mathbus:mbnode-subterm node 1))
		 (convert-to-weyl (mathbus:mbnode-subterm node 2))))
	  ((eql label mathbus::MBS_Application)
	   (let ((w-terms nil))
	     (mathbus:loop-over-subterms node (i type)
	       (unless (eql i 1)
		 (push (convert-to-weyl (mathbus:mbnode-subterm node i))
		       w-terms)))
	     (apply #'make-ge-funct
		    *general*
		    (lookup-weyl-funct (mathbus::mbnode-subterm node 1)
				       (1- (mathbus:mbnode-nSubterms node)))
		    w-terms)))
	  (t (error "Don't know what to do with ~S" node)))))

(defun lookup-weyl-funct (node nargs)
  (cond ((eql (mathbus:mbnode-label node) mathbus:MBS_AlgFuncCode)
	 (make-function *general*
			(gethash (mathbus:mbnode-subterm node 1) 
                                  *weyl-func-table*)
			nargs))
	(t (error "Not yet"))))