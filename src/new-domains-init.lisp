;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			  Algebraic Domains Initialization
;;; ===========================================================================
;;; (c) Copyright 1994, 1997 Cornell University

;;; $Header:$


(in-package "USER")

;;; ====================
;;; operator definitions
;;; ====================

;; The following forms define the allowable operators.  The second
;; argument is the number arguments of the operator.

(define-math-operator zero (0))
(define-math-operator minus (1))
(define-math-operator difference (2))
(define-math-operator plus (2)
  :identity zero
  :unary-inverse minus
  :binary-inverse difference)
    
(define-math-operator one (0))
(define-math-operator recip (1))
(define-math-operator quotient (2))
(define-math-operator times (2)
  :identity one
  :unary-inverse recip
  :binary-inverse quotient)

;;; ====================
;;; property definitions
;;; ====================

;; The terms we use are all of the form:
;;
;;		  (SYMBOL value1 value2 .... valuen)
;;
;; The values in a property can be of one of four types:
;; (1) A symbol.  This usually (always?) the symbol corresponding to a
;;     generic function.
;; (2) An (LISP) integer.  Used to represent the characteristic of a
;;     domain (etc.) 
;; (3) A domain.
;; (4) A domain element.


;; This property indicates that for that operator maps pairs of
;; elements of domain into domain.
(define-primitive-property has-equality (domain &optional (equal 'binary=))
  )

(define-primitive-property has-comparison (domain &optional (great 'binary>))
  )

(define-primitive-property has-binary-operation (domain operator)
  )

(define-primitive-property associative (domain operator)
  )

(define-primitive-property commutative (domain operator)
  )

;; A unit e is such that e * a = a, whenever the multiplication is legal.
(define-primitive-property has-identity (domain op &optional ident)
  )

;; An annihilator e is such that e * a = e
;; There can only be one annhilator
(define-primitive-property has-annihilator (domain operator ann)
  )

;; The has-inverse property means (op x (inv op)) is equal to
;; the identity of op. 
;; We are not defining left and right inverse...
(define-primitive-property has-inverse
    (domain op &optional (inv (unary-inverse-of op)))
  )

;; We name the operators times and plus to make clear who ditributes over who
(define-primitive-property distributes (domain times plus)
  )

(define-primitive-property has-zero-divisors (domain operator)
  )

(define-primitive-property complete (domain)
  )
  
;; Indicates that the operation great orders the elements of the domain.
;; Need to have partial orders etc. 
(define-primitive-property ordered (domain &optional (great 'binary>))
  )

;; Need to deal with multiple derivations also
(define-primitive-property has-derivation (domain deriv)
  )

;; Means that every pair of elements has a greatest common divisor
;; (with respect to the operator times)
(define-primitive-property euclidean (domain times)
  )

(define-primitive-property unique-factorization (domain times)
  )

(define-primitive-property has-factorization (domain times factor)
  )

;; The following indicates that (action <dom2> <domain>) => <domain>

(define-primitive-property left-action (domain dom2 action)
  )

(define-primitive-property right-action (domain dom2 action)
  )

(define-math-property action (domain dom2 action)
  (left-action domain dom2 action)
  (right-action domain dom2 action))
  

;; Mathematical constructs

;; Each element of the argument list can be either an atom, or list
;; consisting of the name of the argument followed by keyword, value
;; pairs.  The allowable keywords are:

;; :default          Only allowable in optional arguments.  This keyword is
;;                   used to provide a bit more uniformity in the syntax. 
;;
;; :predicate-optional   This argument is not required for predicates


;; The body is a list of properties.  They can be either:
;;
;; (1) (prop-name a b c)   A full property, in which the property is asserted
;;                         as true, ie. the value generate is T.
;; (2) (not (prop-name ...))  Value is asserted as NIL
;; (3) ((prop-name ...) :value value)  Value is value.

;; The first form in the body of DEFINE-MATH-PROPERTY can begin with
;; ASSERT-EXPRESSIONS or PREDICATE-EXPRESSIONS.  The (progn) body of
;; this form is a set of expressions that will be evaluated at the
;; beginning of all generated ASSERT (!) and PREDICATE (?) forms.

(define-primitive-property set (domain)
  )

(define-math-property semigroup (domain operator)
  (set domain)
  (has-binary-operation domain operator)
  (associative domain operator))

(define-math-property monoid (domain op &optional (ident (identity-of op)))
  (semigroup domain op)
  (has-identity domain op ident))

(define-primitive-property all-elements-have-inverses (domain op)
  )

(define-math-property group (domain op
				    &optional (ident (identity-of op))
				    (inverse  (unary-inverse-of op)))
  (monoid domain op ident)
  (has-inverse domain op inverse)
  (all-elements-have-inverses domain op))

(define-math-property abelian-group (domain op
				    &optional (ident (identity-of op))
				    (inverse  (unary-inverse-of op)))
  (group domain op ident inverse)
  (commutative domain op))

;; The plus and times operators are made optional since it is
;; exceedingly rare that we don't want to use plus and times as the
;; basic operators of the ring.
(define-math-property ring (domain &optional (plus 'plus) (times 'times))
  (group domain plus)
  (commutative domain plus)
  (semigroup domain times)
  (distributes domain times plus))

;; Commutative ring with unit, actually
(define-math-property commutative-ring (domain &optional
					       (plus 'plus) (times 'times))
  (ring domain plus times)
  (has-identity domain times)
  (commutative domain times))

(define-primitive-property all-nonzero-elements-have-inverses (domain op)
  )

(define-math-property field (domain plus times)
  (commutative-ring domain plus times)
  (has-inverse domain times)
  (all-nonzero-elements-have-inverses domain times))


(define-primitive-property has-coefficient-domain (domain coef-domain)
  )

(define-math-property module (domain plus times coefficient-domain)
  (group domain plus)
  (commutative domain plus)
  (has-coefficient-domain coefficient-domain)
  (action domain times coefficient-domain))

(define-primitive-property hausdorff (domain)
  )

(define-primitive-property has-dimension (domain d)
  )

(define-primitive-property has-coordinate-systems (domain)
  )

(define-math-property locally-euclidean (domain d)
  (has-dimension domain d)
  (has-coordinate-systems domain))  

(define-math-property manifold (domain d)
  (hausdorff domain)
  (locally-euclidean domain d))


;;; ===============
;;; Domain Creators
;;; ===============

(define-domain-creator ring-of-fractions
    (domain ring &optional (multiplicative-set ring))
  (cond ((and (rational-integers? ring)
	      (eql multiplicative-set ring))
	 (setf (pretty-name-of domain) "Q"))
	(t (setf (pretty-name-of domain)
		 (format nil "~A/~A" ring multiplicative-set)))))

(define-domain-creator polynomial-ring (domain coefs vars)
  (ring! domain)
  (setf (pretty-name-of domain) (format nil "~A[~S~{, ~S~}]" coefs
					(first vars) (rest vars))))


(define-domain-creator ideal (domain &key ring generators)
  (setf (pretty-name-of domain)
        (format nil "(~S~{, ~S~})" (first generators) (rest generators))))

(define-domain-creator quotient-ring (domain ring ideal)
  (setf (pretty-name-of domain)
        (format nil "~A/~A" ring ideal)))
 



