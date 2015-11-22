;; -*- Base: 10; Mode: Lisp; Syntax: Common-lisp; Package: weyl; Lowercase: T -*-

;;  Some example meshes.

(in-package 'weyl)

(defun square (&key (size-list '(inside .4 outside .2)) (angle-bounds '(30)))
  (make-mesh ((get-euclidean-space 2)
	      :size-list size-list :angle-bounds angle-bounds)
	     (region inside .5 .5)
	     (boundary outside (:closed? t)
		       line (pt 0 0) (pt 1 0) (pt 1 1) (pt 0 1))))

;;  This function will read a mesh-request file and create the
;;  corresponding mesh.  Example mesh-request files are stored in
;;  `weyl/demo/square-mesh.mr' and in `weyl/demo/circle-mesh.mr'.  The
;;  resulting mesh from the square-mesh example is stored in file
;;  `weyl/demo/square-mesh.mesh' (via use of the function write-mesh).
;;  The function read-mesh can be used to bring the .mesh file back
;;  into memory as a mesh.
(defun try-make-mesh-from-file (file-name)
    (with-open-file (in-file file-name :direction :input)
      (make-mesh-from-file in-file)))

;;  Creates a 3/4 circle that is meshed down to small size at the center.
(defun circle-max-size (x y)
  (flet ((sqr (x) (* x x)))
    (let ((h0 .4))
      (* h0 (sqrt (+ (sqr x) (sqr y) (sqr (* h0 h0 h0))))))))

(defun three-quarter-circle (&key (angle-bounds '(30))
				  (size-list (list 'inside #'circle-max-size
						   'outside .25)))
  (make-mesh ((get-euclidean-space 2)
	      :size-list size-list :angle-bounds angle-bounds)
	     (region inside -0.1 0)
	     (boundary outside (:closed? t) arc
		       (pt 0 0) () (pt 1 0) (:radius 1) (pt 0 1) (:radius 1)
		       (pt -1 0) (:radius 1) (pt 0 -1) ())))

;;  A reconstruction of an example due to Shephard.
(defun shephard (&key (size-list '(left 30 right 30 center 30))
		      (angle-bounds '(30)))
  (make-mesh ((get-euclidean-space 2)
	      :size-list size-list :angle-bounds angle-bounds)
	     (region inside 205 155)
	     (point center 500 350)
	     (boundary frame (:closed? t) line
		       (pt 200 150) (pt 200 550) (pt 800 550) (pt 800 150))
	     (boundary right (:closed? t :split 2) arc
		       (pt 545 305) (:radius 0)
		       (pt 579 271) (:center center)
		       (pt 579 429) (:radius 0)
		       (pt 545 395) (:center center :clockwise t))
	     (boundary left (:closed? t :split 2) arc
		       (pt 455 305) (:radius 0 :cw t)
		       (pt 421 271) (:center center :cw t)
		       (pt 421 429) (:radius 0 :cw t)
		       (pt 455 395) (:center center))
	     (boundary center (:closed? t) arc
		       (pt 540 350) (:center center)
		       (pt 500 390) (:center center)
		       (pt 460 350) (:center center)
		       (pt 500 310) (:center center))))

;;  Looks like a keyhole.
(defun keyhole (&key (size-list '(inside 50 curve 25)) (angle-bounds '(30)))
  (make-mesh ((get-euclidean-space 2)
	      :size-list size-list :angle-bounds angle-bounds)
	     (region inside 251 51)
	     (point left 480 350)
	     (point right 520 350)
	     (boundary frame (:closed? t) line
		       (pt 250 50) (pt 250 600) (pt 750 600) (pt 750 50))
	     (boundary straight (:closed? nil) line
		       left (pt 450 200) (pt 550 200) right)
	     (boundary curve (:closed? nil :split 3) arc
		       right (:thru (pt 500 450)) left)))
  
;;  An example that looks like a pumpkin.
(defun pumpkin (&key (size-list '(inside 100)) (angle-bounds '(30)))
  (make-mesh ((get-euclidean-space 2)
	      :size-list size-list :angle-bounds angle-bounds)
	     (region inside 524 739)
	     (point left 475 740)
	     (point right 525 740)
	     (boundary stem (:closed? nil) line
		       left (pt 480 790) (pt 530 790) right)
	     (boundary circle (:closed? nil :split 3) arc
		       left (:thru (pt 500 120)) right)
	     (boundary nose (:closed? t) line
		       (pt 500 460) (pt 445 360) (pt 555 360))
	     (boundary left-eye (:closed? t) line
		       (pt 350 560) (pt 295 460) (pt 405 460))
	     (boundary right-eye (:closed? t) line
		       (pt 650 560) (pt 595 460) (pt 705 460))
	     (boundary mouth (:closed? t :split 2) arc
		       (pt 300 350) (:thru (pt 500 280))
		       (pt 700 350) (:thru (pt 500 180) :cw t))))

;;  A circle with an internal boundary.
(defun circle-with-boundary (&key (size-list '(frame .5 top .3 bottom .1))
				  (angle-bounds '(30)))
  (make-mesh ((get-euclidean-space 2)
	      :size-list size-list :angle-bounds angle-bounds)
	     (region top 0 .5)
	     (region bottom 0 -.5)
	     (region outer 0 1.5)
	     (point center 0 0)
	     (point left -1 0)
	     (point right 1 0)
	     (boundary frame (:closed? t :split 3) arc
		       (pt -2 0) (:center center) (pt 2 0) (:center center))
	     (boundary top-b (:closed? nil :split 2) arc
		       right (:center center) left)
	     (boundary bottom-b (:closed? nil :split 2) arc
		       left (:center center) right)
	     (boundary across (:closed? nil) line
		       left right)))

;;  A circle with a crack in it.  Using ":size-list (list 'inside
;;  #'cracked-max-size)" will cause a mesh with greater density on one
;;  side.
(defun cracked-max-size (x y)
  (if (and (< x 0) (< y 0)) .1 .4))

(defun cracked (&key (size-list (list 'inside #'cracked-max-size))
		     (angle-bounds '(30)))
  (make-mesh ((get-euclidean-space 2)
	      :size-list size-list :angle-bounds angle-bounds)
	     (region inside .1 .1)
	     (point mid 0 -1)
	     (point center 0 0)
	     (boundary outside (:closed? t :split 3) arc
		       mid (:center center) (pt 0 1) (:center center))
	     (boundary crack (:closed? nil) line
		       mid center)))

;;  Creates a Joukowski airfoil (well-known complex transformation of
;;  circle into airfoil).  The parametric generating function is
;;  returned by airfoil-generator; changing the center changes the
;;  shape of the airfoil.  The center should be a complex number near
;;  the interval -1 < x < 0 (it doesn't have to be on the interval).
;;  The transformation is w = z + 1/z.
(defun airfoil-generator (center target-space)
    (let* ((z0 center)
	   (tip-vector (cl:- 1 z0))
	   (r (cl:abs tip-vector))
	   (theta0 (cl:phase tip-vector)))
      #'(lambda (theta)
	  (let* ((theta (convert-to-lisp-number theta))
		 (z (cl:+ z0 (cl:* r (cl:cis (cl:+ theta0 theta)))))
		 (w (cl:+ z (cl:/ z))))
	    (make-element target-space (cl:realpart w) (cl:imagpart w))))))

;;  The curved-segment that is the airfoil must be broken into two
;;  parts.  This is to ensure that the endpoint "end" is stored with
;;  different values.  The first segment stores end as angle 0 the
;;  second stores end as angle 2pi.  This type of splitting is needed
;;  whenever a single vertex must act as if it has two different
;;  coordinates.
(defun j-airfoil (&key (size-list '(inside .5 airfoil .05))
		       (angle-bounds '(30)))
  (loop with space = (get-euclidean-space 2)
	with mesh = (create-mesh space)
	with generator = (airfoil-generator (cl:complex -0.15 0.15) space)
	with end = (make-point space (funcall generator 0))
	with middle = (make-point space (funcall generator cl:pi))
	with part1 = (make-curved-segment space 0 end cl:pi middle generator)
	with part2 = (make-curved-segment
			  space cl:pi middle (+ cl:pi cl:pi) end generator)
	with boundaries = (list part1 part2)
	with corner = (make-point space -3 1.5)
	with frame = (list corner
			   (make-point space -3 -1.5)
			   (make-point space 3 -1.5)
			   (make-point space 3 1.5) corner)
	repeat 4
	do (setf boundaries (split boundaries nil))
	finally
     (loop for a in frame and b in (rest frame)
	   do (insert-boundary (make-simplex a b) mesh :name 'frame))
     (loop for b in boundaries
	   do (insert-boundary b mesh :name 'airfoil))
     (name-region 'inside (make-point space 2.9 1.4) mesh)
     (refine-mesh mesh :size-list size-list :angle-bounds angle-bounds)))

