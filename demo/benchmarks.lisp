;;   -*- Syntax: Common-Lisp; Package: weyli; Base: 10; Mode: LISP -*-

(in-package :weyli)
;;; To use the functions in this file, load it and then either select
;;; commands from the comment block at the end of this file, or type
;;; (weyli::benchmark) at the lisp listener.

;;; This file contains functions for testing the performance of
;;; combinatorial data structures and operators in Weyl.
;;;
;;; The tests are to create a simplicial decompositions of
;;; k-dimensional cubes in r^k (for k = 1, 2, and 3 dimensions), define
;;; chains on those complexes, and compute the boundary of those
;;; chains
;;;

;;; First some functions to make it easier to create simplicial
;;; complexes from lists of vertices that are represented as lists of
;;; lisp numbers.  These functions take are of mapping into the weyl
;;; structures.

;;; A slightly higher interface for creating simplices (than
;;; MAKE-SIMPLEX).  This version of make-simplex creates POINT
;;; versions of its input, checking to see whether one has already
;;; been created by using a hash table


(defvar *displaying* nil)

(defmethod coordinate-list ((list list))
  list)


;;; A slightly higher interface for creating simplicial complexes.
;;; This one creates a domain if need be, and maps lisp-list version
;;; of coordinates into points.

(defun make-simplex2 (vert-list domain &optional ht)
  (apply #'make-simplex
	 (loop for v in vert-list collect
	       (or (gethash v ht)
		   (setf (gethash v ht)
		       (apply #'make-point (cons domain v)))))))
(defun create-simplicial-complex (verts-list &key (domain nil))
  (or domain (setf domain (get-euclidean-space (length (first(first verts-list))))))
  (let* ((ht (make-hash-table :test #'equal))
	 (simps (loop for vl in verts-list collect
		      (make-simplex2 vl domain ht)))
	 (self (make-simplicial-complex simps)))
    self))

(defmethod k-cells ((cm cell-complex) k)
  (let ((ret-val nil))
    (map-over-cells (cell k)  cm
	      (push cell ret-val))
    ret-val))

(defmethod lisp-number ((number number))
  number)

(defmethod lisp-number ((list list))
  (mapcar #'lisp-number list))

(defmethod lisp-number ((weyl-number numeric))
  (convert-to-lisp-number weyl-number))

(defmethod lisp-number ((weyl-number vector-space-element))
  (loop for i below (dimension-of weyl-number) collect
	(lisp-number (ref weyl-number i))))




;;;The benchmarking codes follow

;;;create N0 1-simplices
(defun simps-for-cube1 (n0)
  (loop for a below n0 collect
	`((,a 0) (,(1+ a) 0))))


;;;create N0xN1x2 2-simplices
(defun simps-for-cube2 (n0 n1)
  (loop for a  below n0 nconc
	(loop for b below n1 nconc
	      (list
	       (list (list a b) (list (1+ a) b)
		     (list a (1+ b)))
	       (list (list a (1+ b))
		     (list (1+ a) b)
		     (list (1+ a) (1+ b)))))))

(defun simps-from-3cube (v0 v1 v2 v3 v4 v5 v6 v7)
  (list  
   (list v0 v1 v2 v4)
   (list v1 v2 v4 v5)
   (list v1 v2 v3 v7)
   (list v1 v2 v6 v7)
   (list v1 v5 v6 v7)
   (list v2 v4 v5 v6)))

;;;create N0xN1xN2x6 3-simplices
(defun simps-for-cube3 (n0 n1 n2)
  (loop for a  below n0 nconc
	(loop for b below n1 nconc
	      (loop for c below n2 nconc
		    (loop for simp in
			  (simps-from-3cube
			   (list a b c)
			   (list (1+ a) b c)
			   (list a (1+ b) c)
			   (list (1+ a) (1+ b) c)
			   (list a b (1+ c))
			   (list (1+ a) b (1+ c))
			   (list a (1+ b) (1+ c))
			   (list (1+ a) (1+ b) (1+ c)))
			   collect simp)))))


 ;;; two timing functions, one using elapsed time, the other using the
 ;;; lisp TIME function.  The former is useful if you want to use the
 ;;; output for creating graphs, etc.  The latter prints GC info as
 ;;; well.

 '(defmacro mytime (mess count expr &optional (stream *standard-output*))
    `(let ((before nil)
	   (ret-val nil)
	   (elapsed-time nil))
       (setf before  (GET-INTERNAL-REAL-TIME))
       (setf ret-val ,expr)
       (setf elapsed-time (/(- (GET-INTERNAL-REAL-TIME) before)
			    INTERNAL-TIME-UNITS-PER-SECOND))
       (format ,stream
	       "~a~%Total: ~gGG Per simplex: ~g~%" 
	       ,mess
	       elapsed-time
	       (/ elapsed-time ,count) 
	       )
       ret-val))


 (defmacro mytime (mess count expr &optional (stream *standard-output*))
   (declare (ignore count))
   `(progn
      (format ,stream "~%~a"   ,mess)
      (time ,expr)))





 ;;; The main test function -- given a set of simplcies, creates a
 ;;; simplicial complex, displays if desired, creates a
 ;;; maximal-dimension chain over the complex (each simplex has
 ;;; coefficient 1) and creates the boundary of the chain. (Each
 ;;; interior (k-1)-cell has coefficient 0, others are either 1 or -1.)
(defun benchmark-complex
  (simps &key (coefficient-domain(get-rational-integers))
	 (display nil)
	 (stream *standard-output*))
  (let ((len (length simps))
	(dim (1-(length(first simps))))
					;(complex nil)
					;(domain nil)
	(chain-pairs nil)
	(cochain-pairs nil)
					;(chain nil)
					;(cochain nil)
	)
    (format stream "~%Timings for ~d-complex with ~d simplices(in seconds):~%"
	    dim len)
    (mytime
     (format nil "Create ~d-complex with ~d simplices:"
	     dim len)
     len
     (setf complex (create-simplicial-complex simps))
     stream)
    
    (when display
      #-gfx(error "must (load \"~/simlab/weyl/demo/draw-topology.lisp\") and
             (init-gfx <yourXserverName>) to use display")
      (format stream "Display ~d-complex with ~d simplices:"
	      dim len)
      (look complex)
      (draw-all complex :oriented t))
    
    
    (setf domain (get-chain-module complex dim coefficient-domain))
    
    (mytime
     (format nil "Create chain input for ~d-chain over ~d simplices:"
	     dim (length simps))
     len
     (map-over-cells
      (cell dim) complex
      (push (cons cell (one coefficient-domain)) chain-pairs))
     stream)
    
    
    (mytime
     (format nil "Create  ~d-chain from ~d pairs:" dim len)
     len
     (setf chain (make-chain domain chain-pairs))
     stream)
    
    (when display
	  (format stream "Display ~d-chain of dimension ~d:"
		  dim len)
	  (draw chain))
    
    (when (plusp dim)
	  (mytime
	   (format nil "Compute boundary of ~d-chain with ~d simplices:"
		   dim len)
	   len
	   (setf bound (boundary chain))
	   stream)
	  
	  (when display
		(format stream "Display boundary (~d-chain of dimension ~d):"
			dim (length (k-cells complex (1- dim))))
		(draw bound)))
    (format stream "~%")
    
    (setf domain (get-cochain-module complex 0 coefficient-domain))
    (setf len (length (k-cells complex 0)))
    (mytime
     (format nil "Create cochain input for 0-chain over ~d simplices:"
	     len)
     len
     (map-over-cells
      (cell 0) complex
      (push (cons cell (one coefficient-domain)) cochain-pairs))
     stream)
    (mytime
     (format nil "Create  ~d-cochain from ~d pairs:" 0 len)
     len
     (setf cochain (make-cochain domain cochain-pairs))
     stream)
    
    (when display
	  (draw-all complex)
	  (format stream "Display ~d-cochain of dimension ~d:"
		  0 len)
	  (draw cochain))
    (when (plusp dim)
	  (mytime
	   (format nil "Compute coboundary of ~d-cochain with ~d simplices:"
		   0 len)
	   len
	   (setf cobound (coboundary cochain))
	   stream)
	  
	  (when display
		(format stream "Display coboundary (~d-cochain of dimension ~d):"
			1 (length (k-cells complex 1)))
		(draw cobound )))
    (format stream "~%")
    ))


(defun demo-pathname(filename)
  (make-pathname
   :device (pathname-device user::*central-registry*)
   :directory (append (butlast (pathname-directory
				user::*central-registry*))
		      (list "weyl" "demo"))
   :name filename))

(defun demo-load (filename)
  (load (demo-pathname filename)))

(defun test-topology (&key (num-steps1 30)
			   (num-steps2 25)
			   (num-steps3 20))
  ;; the following three LOOP forms test out the topology code for 1, 2,
  ;; and 3 complexes of various sizes.
  (loop for i from 1 below num-steps1 do
      (benchmark-complex (simps-for-cube1 (* i i i))))

  (loop for i from 1 below num-steps2 do
	(benchmark-complex (simps-for-cube2 i (* i i))))
  
  (loop for i from 1 below num-steps3 do
	(benchmark-complex (simps-for-cube3 i i i))))

(defmacro test-mesh(body)
  `(progn
     (time(setf *mesh* ,body))
     (if *displaying* (look *mesh* :position '(0 0 1000)))))

(defun test-meshing ()
  (demo-load "mesh-examples")
  (test-mesh(weyl::square))
  (test-mesh(weyl::try-make-mesh-from-file (demo-pathname "circle-mesh.mr")))
  (test-mesh(weyl::three-quarter-circle))
  (test-mesh(weyl::shephard))
  (test-mesh(weyl::keyhole))
  (test-mesh(weyl::pumpkin))
  (test-mesh(weyl::circle-with-boundary))
  (test-mesh(weyl::cracked))
  (test-mesh(weyl::j-airfoil)))

;;; test Weyl arithmetic
(defun test-arithmetic()
  (demo-load "arith")
  (weyl::test-plus)
  (weyl::test-quotient)
  (weyl::test-truncate))

;;; test Weyl grobner bases
(defun test-grobner ()
  (demo-load "grobner")
  (weyl::do-grobner-tests '(1 3 4 5 6 7 8 9 10 11))
  )

(defun test-fg ()
  (demo-load "fg-series")
  (loop for n from 5 to 30 by 5 do
	(weyl::time-fg n)))

(defun test-all ()
  (test-topology)
  (test-meshing)
  (test-arithmetic)
  (test-grobner)
  (test-fg))

#|
;;;if you want to view meshing, complexes and chains load the following
;;;progn

(progn
  (demo-load "draw-topology")
  (init-gfx) ;(init-gfx :host "YourXserverMachineName")
  (setf *displaying* t))

(test-all)
(test-topology)
(test-meshing)
(test-arithmetic)
(test-grobner)
(test-fg)



;; take a look at things a bit
(benchmark-complex (simps-for-cube3 1 2 1))
(look complex :position '(150 180 100) :scale 0.4)
(draw-all complex :which-cells '(0 1) :oriented t)
(draw cochain)
(draw (coboundary cochain))

(look complex :position '(150 180 100) :scale 0.4)
(draw-all complex :which-cells '(1) :oriented t)
(setf domain (get-cochain-module
	      complex 1
	      (setf coefficient-domain
		    (get-rational-integers))))
(setf cochain-pairs nil)
(map-over-cells
 (cell 1) complex
 (push (cons cell (coerce (cl::random 10) coefficient-domain)) cochain-pairs))
(setf cochain (make-cochain domain cochain-pairs))
(draw cochain :color *red*)
(draw (setf cb (coboundary cochain)) :color *blue*)
(draw (setf ccb (coboundary cb)) :color *black* :clear t)

|#
