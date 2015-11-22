;;; -*- Mode:Lisp; Package:Weyli; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			           Sets 
;;; ===========================================================================
;;; (c) Copyright 1989, 1997 Cornell University

;;; $Header: sets.lisp,v 1.6 1995/05/24 17:42:11 rz Exp $

(in-package "WEYLI")

;; Tuples are just indexed lists.
(defclass tuple ()
  ((value :initarg :values
	  :initform ()
	  :reader value-of)))

