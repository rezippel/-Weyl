;;; ===========================================================================
;;;			    Weyl System Definition
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; $Id: weyl.system,v 1.10 1995/07/07 18:31:04 mardis Exp $

(in-package :asdf)

(defsystem "weyl"
  :description "Weyl: The Categorical Symbolic Math Package"
  :version "0.5"
  :author "Richard Zippel <rz@alum.mit.edu>"
  :license "GPLv2"
  :depends-on (mathbus)
  :components
   ((:file "packages")
    (:file "maintenance")
    (:file "lisp-support" :depends-on ("packages"))
    (:file "domain-support" :depends-on ("lisp-support"))
    ;; All defclass definitions are included in this module.  This
    ;; eliminates some painful dependencies.
    (:module classes
	     ;; The following prevents assuming this is a subdirectory
	     :pathname #p""
	     :depends-on ("domain-support")
	     :components ((:file "set-classes")
			  (:file "algebraic-domains")
			  (:file "space-classes")
			  (:file "general-classes")))
    (:file "avl" :depends-on (classes))
    (:file "lisp-numbers" :depends-on (classes))
    (:file "sets" :depends-on (classes))
    (:file "morphisms" :depends-on (classes "avl"))
    (:file "quotient-fields" :depends-on (classes)) 
    (:module general
	     :pathname #p""
	     :depends-on (classes)
	     :components ((:file "general")
			  (:file "fourier")
			  (:file "mathbus")))
    (:file "functions" :depends-on (classes general))
    (:file "direct-sums" :depends-on (classes))
    (:file "sequence" :depends-on (classes))
    (:module numbers
	     :pathname #p""
	     :depends-on (classes)
	     :components ((:file "bigfloat")
			  (:file "numbers" :depends-on ("bigfloat"))
			  (:file "gfp")))
    (:module polynomials
	     :pathname #p""
	     :depends-on (classes)
	     :components
	       ((:file "poly-tools")
		(:file "mpolynomial" :depends-on ("poly-tools"))
		(:file "upolynomial" :depends-on ("poly-tools"))
		(:file "epolynomial" :depends-on ("poly-tools"))
		(:file "sparsegcd" :depends-on ("mpolynomial"))
		(:file "grobner" :depends-on ("mpolynomial" "epolynomial"))))
    (:file "tpower" :depends-on (polynomials))
    (:file "taylor" :depends-on (tpower))
    (:file "rational-functions" :depends-on (polynomials "quotient-fields"))
    (:file "differential-domains" :depends-on (polynomials))
    (:file "algebraic-extension" :depends-on (polynomials))

    (:module vector-spaces
	     :pathname #p""
	     :depends-on ("sets")
	     :components
	       ((:file "vector")
		(:file "projective-space" :depends-on ("vector"))
		(:file "quaternions" :depends-on ("vector"))))
    (:file "matrix" :depends-on ("morphisms"))
    (:file "topology" :depends-on ("avl" polynomials vector-spaces))
    ;; Really on space-classes
    (:file "funct-spaces" :depends-on (classes vector-spaces))
    (:file "mesh" :depends-on ("topology"))))

;; This should be a final operation on asdf file, but I don't know how
;; to do that yet.
(defun initialize-weyl ()
  (pushnew :weyl *features*)
  (funcall (intern "INITIALIZE-CONTEXTS" 'weyli))
  (funcall (intern "RESET-DOMAINS" 'weyli))
  #+ignore
  (print-system-banner 'weyl t)
  (in-package :weyl))
