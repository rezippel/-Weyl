;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;                                 Sequences
;;; ===========================================================================
;;; (c) Copyright 1995, 1997 Cornell University

;;; $Header: sequence.lisp,v 1.1 1995/07/07 18:11:46 mardis Exp $

(in-package "WEYLI")

(defclass integer-sequence ()
     ((start :initform 1
	     :initarg :start
	     :accessor start-of)
      (end :initform nil
	   :initarg :end
	   :accessor end-of)
      (step :initform 1
	    :initarg :step
	    :accessor step-of)))

(defmethod print-object ((i integer-sequence) stream)
   (flet ((print-with-infs (x)
           (cond ((eql x weyl:*positive-infinity*)
                  "inf")
                 ((eql x weyl:*negative-infinity*)
                  "-inf")
                 (t x))))
    (write-char #\[ stream)
    (if (ge-equal 1 (step-of i))
      (format stream "~A:~A"
	      (print-with-infs (start-of i))
	      (print-with-infs (end-of i)))
      (format stream "~A:~A:~A"
	      (print-with-infs (start-of i))
	      (print-with-infs (step-of i))
	      (print-with-infs (end-of i))))
    (write-char #\] stream)))

(defmethod make-integer-sequence (start end &key (step 1))
  (make-integer-sequence (coerce start *general*)
			 (coerce end *general*)
			 :step step))

(defmethod make-integer-sequence ((start number) end &key (step 1))
  (make-instance 'integer-sequence
		 :start start
		 :end (coerce end *general*)
		 :step step))

(defmethod make-integer-sequence (start (end number) &key (step 1))
  (make-instance 'integer-sequence
		 :start (coerce start *general*)
		 :end end
		 :step step))

(defmethod make-integer-sequence ((start number)
                                 (end number)
                                 &key (step 1))
  (make-instance 'integer-sequence
                 :start start
                 :end end
                 :step step))

(defmethod make-integer-sequence ((start domain-element)
                                 (end domain-element)
                                 &key (step 1))
  (make-instance 'integer-sequence
                 :start start
                 :end end
                 :step step))

(defmethod intersection ((i1 integer-sequence)
                         (i2 integer-sequence) &rest rest)
  (when rest
    (error "Too many arguments to Intersection."))
  (if (ge-equal (step-of i1) (step-of i2))
    (if (> 0 (step-of i1))
      (make-sequence (min (start-of i1) (start-of i2))
		     (max (end-of i1) (end-of i2)))
      (make-sequence (max (start-of i1) (start-of i2))
		     (min (end-of i1) (end-of i2))))
    nil))

(defmethod union ((i1 integer-sequence)
                  (i2 integer-sequence) &rest rest)
  (when rest
    (error "Too many arguments to Union."))
  (if (ge-equal (step-of i1) (step-of i2))
    (if (< 0 (step-of i1))
      (make-sequence (min (start-of i1) (start-of i2))
		     (max (end-of i1) (end-of i2)))
      (make-sequence (max (start-of i1) (start-of i2))
		     (min (end-of i1) (end-of i2))))
    nil))


