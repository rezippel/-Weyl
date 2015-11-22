;;;   -*- Syntax: Common-Lisp; Package: Weyli; Base: 10; Mode: LISP -*-

(in-package :weyli)

(unless (find-package "COMMON-LISP-USER")
  (MAKE-PACKAGE "COMMON-LISP-USER" :nicknames (list "CLU")))

(import '(clu::*green* clu::*red* clu::*blue* clu::*black* clu::*orange* 
		       clu::*white* clu::clear-display clu::x-force-output
		       clu::*display* clu::*foreground* clu::draw-edge
		       clu::draw-arrow-arc
		       clu::draw-coordinate-frame clu::*background*
		       clu::*viewport* clu::with-postscript
		       clu::*DEFAULT-FONT* clu::draw-string
		       user::get-system clu::with-line-width
		       clu::init-gfx))

(make:load-system 'gfx)



(defvar *print-tol* 1e-3)
(defvar *box-default* nil)
(defvar *box-chains* nil
  "Whether or not chain coefficients should be displayed with boxes
   around them")


(defvar *display-faces* nil)

(defvar *oriented-edges* nil)


(defun clu::coords-of (p)
  (coordinate-list p))
   


(defun clu::bbox(anything) ;;; for gfx
  (let ((bb(bbox anything)))
    (list (coordinate-list (min-of bb))
	  (coordinate-list (max-of bb)))))

(defun clu::vertices-of(anything) ;;; for gfx
  (vertices-of anything))

(defclass bbox ()
  ((min :initform nil :initarg :min :accessor min-of)
   (max :initform nil :initarg :max :accessor max-of))
  (:documentation "A bounding box represented by it's min and max points."))

(defun make-bbox (min max)
  (make-instance 'bbox :min min :max max))

(defmethod bbox ((any-old-thing t) &optional (bb nil))
  "The default is: IDENTITY"
  bb)
(defmethod bbox ((v vector-space-element) &optional (bb nil))
  (if bb
      (setf (min-of bb) (min (min-of bb) v)
	    (max-of bb) (max (max-of bb) v))
    (setf bb (make-bbox v v)))
  bb)

(defmethod bbox ((list list) &optional (bb nil))
  "Compute a bounding box for CELL"
  (loop for item in list do
	(setf bb (bbox item bb)))
  bb)

(defmethod bbox ((cell cell) &optional (bb nil))
  "Compute a bounding box for CELL"
  (bbox (vertices-of cell) bb))

(defmethod bbox ((complex cell-complex) &optional (bb nil))
  "Compute a bounding box for CELL"
  (map-over-cells (cell 0) complex
		    (setf bb (bbox cell bb)))
  bb)

(defmethod bbox ((p point) &optional (bbox nil))
  (or bbox
      (setf bbox
	    (let ((zero (zero(domain-of p)))) 
	      (make-instance 'bbox :min (+ p zero) :max (+ p zero)))))
  (with-slots
   (min max) bbox
   (loop for i below  (dimension-of (domain-of p)) do
	 (setf (ref min i) (min (ref p i) (ref min i)))
	 (setf (ref max i) (max (ref p i) (ref max i)))
	 ))
  bbox)

(defmethod lbbox ((any t) &optional (bb nil))
  "Compute a bounding box for CELL"
  (declare (ignore bb))
  (let ((bbox (bbox any)))
    (list (lisp-number (min-of bbox)) (lisp-number (max-of bbox)))))


(defmethod screen-format ((val t))
  val)

(defmethod screen-format :around ((val t))
  (let ((*show-weylism* nil))
    (declare (special *show-weylism*))
    (call-next-method)
    ))

(defmethod screen-format ((val point))
  (loop with a = (value-of val)
	for i below (dimension-of val)
	collect (* (round (aref a i)  *print-tol*) *print-tol*)))

(defmethod color-of ((chain chain))
  (color-of (complex-of chain)))

(defmethod draw-val ((cell cell) val &key (color nil) (update t)
		     (clear nil) (boxed *box-default*)
		     (viewport *viewport*) (font *default-font*)
		     )
  (if clear (clear-display))
  (let ((point (coordinate-list
		(barycenter cell))))
    (draw-string (format nil "~a" val)
		 point :color color :update update :boxed boxed
		 :viewport viewport :font font)))


(defmethod draw-val ((point list) val &key (color nil) (update t) (clear nil)
		     (boxed nil)
		     (display *display*) (font *default-font*))
  (declare (ignore display))
  (if clear (clear-display))
  (draw-string (format nil "~a" val)
	       point
	       :color color
	       :update update
	       :font font :boxed boxed))

#+apple
(defmethod draw-val ((cell cell) val &key (color nil) (update t) (clear nil)
		     (display *display*) (font *default-font*))
  (if clear (clear-display))
  (let ((point (screen-vertex (barycenter cell) *viewport* nil)))
    (draw-string (format nil "~a" val)
		 (first  point)      
		 (second  point)
		 :color color
		 :update update
		 :display display
		 :font font)))


(defmethod barycenter ((points list) &optional (coords nil))
  "Return the barycenter of a list of points."
  (let* ((len  (length points))
	 (domain (domain-of (first points)))
	 (ret-val (zero (domain-of(first points)))))
    (or coords (setf coords (make-list
			     len :initial-element
			     (/ (one (coefficient-domain-of domain))
				len))))
    (loop for point in points
	  for coord in coords do
	  (setf ret-val (+ ret-val (* coord point))))
    ret-val))

(defmethod barycenter ((bbox bbox) &optional (coords nil))
  "Return the barycenter of a list of points."
  (barycenter (list (min-of bbox) (max-of bbox)) coords))



(defmethod barycenter ((simplex simplex) &optional (coords nil))
  "Return the barycenter of the vertices of the simplex."
  (let* ((len (1+ (dimension-of simplex)))
	 (verts (vertices-of simplex))
	 (domain (domain-of (first verts)))
	 (ret-val (zero (domain-of(first verts)))))
    (or coords (setf coords (make-list len :initial-element
				       (/ (one (coefficient-domain-of domain))
					  len))))
    (loop for vert in verts
	  for coord in coords do
	  (setf ret-val (+ ret-val (* coord vert))))
    ret-val))


(defmethod color-of ((simp simplex))
  (case (dimension-of simp)
    (0 *black*)
    (1 *red*)
    (2 *blue*)
    (3 *green*)
    (t *orange*)))

(defun flash (what  &key (complex nil)
		    (color *black*) (times 3) (time-interval 1/4) (clear nil))
  (if clear (clear-display))
  (loop for i below times do
	(draw what :color color :complex complex)
	(sleep time-interval)
	(draw what :color *white* :complex complex)
	(sleep time-interval)))

(defmethod draw ((cell simplex) &key (color nil) (update t)
		 (clear nil) (complex nil))
  (if clear (clu::clear-display))
  (or color (setf color (color-of cell)))
  (let* ((dimension (dimension-of cell))
	 (vertices (vertices-of cell)))
    (case dimension
	  (0  (draw (first vertices) :color color))
	  (1 (clu::draw-edge cell :color color 
			     :oriented *oriented-edges*))
	  (2 (if *display-faces*
		 (clu::draw-face
		  (mapcar #'coordinate-list cell) :color color)
	       (draw (facets cell complex) :color color :update update)))
	  (t (draw (facets cell complex) :color color :update update))))
  #+xlib(if update (clu::x-force-output)))

(defmethod draw ((l list) &key (color nil) (update t) (clear nil)(complex nil))
  (if clear (clear-display))
  (loop for item in l do
	(draw item :color color :update nil :clear nil :complex complex))
  #+xlib(if update (clu::x-force-output)))

(defmethod draw ((cm cell-complex) &key (color nil) (update t) (clear nil)(complex nil))
  (if clear (clear-display))
  (draw (k-cells cm (max-dimension-of cm)) :complex cm   :color color :update nil
	:clear nil :complex complex)
  #+xlib(if update (clu::x-force-output)))


(defmethod draw ((vert point) &key (color *black*) (update t) (clear nil)
		 (position nil))
  (if position (error "don't expect POSITION"))
  (if clear (clear-display))
  (clu::draw-vertex (coordinate-list vert) :color color)
  (if update (clu::x-force-output)))


(defmethod draw ((chain chain)  &key (color *black*) (update t) (clear nil)
		 (offset nil) (zero t) (boxed *box-chains*)(position nil))
  (if clear (clear-display))
  (let ((z nil)
	(ht (make-hash-table)))
    (loop for (cell . value)  in (chain-terms-of chain) do
	  (setf (gethash cell ht) t)
	  (draw-val cell (screen-format value)
		    :color color
		    :update nil :offset offset :boxed boxed
		    :position position))
    (when zero
      (setf z (screen-format (zero(coefficient-domain-of (domain-of chain))))))
      (loop for cell in (k-cells
			 (complex-of chain)
			 (dimension-of chain))
	    do (unless (gethash cell ht)
		 (draw-val cell z
			   :color color
			   :update nil :offset offset :boxed boxed
			   :position position))))
  (if update (x-force-output)))

(defun imbed-point (list n)
  (loop for i below n
	collect (or (pop list) 0.0)))

	
(defun conform (bbox &key (camera clu::*camera*) (scale 0.9)
		     (update t) (position nil) (orientation nil))
  (let* ((min (min-of bbox))
	 (max (max-of bbox))
	 (target (imbed-point (lisp-number (* 1/2 (+ min max))) 3))
	 (lens 0.0))
    
    (loop for i below (dimension-of min) do
	  (setf lens (max lens (- (ref max i) (ref min i)))))
    (setf lens (* 0.5 (lisp-number lens) (/ scale)))
    (if update
	(clu::update-camera
	 :position position
	 :orientation orientation
	 :target target
	 :lens-width lens
	 :lens-height lens
	 :camera camera))
    (list lens target)))

(defun look-from (where at
			&key (orientation '(1.0 0.0 0.0))
			(color *foreground*)
			(scale 0.9) (offset nil) (position nil))
  (clu::update-camera :position where :orientation orientation)
  (conform (bbox (or position at)) :scale scale)
  (draw at :color color :clear t :offset offset :position position)
  )

(defun look (at-what &key
		     (position nil)
		     (orientation nil)
		     (color *foreground*)
		     (scale 0.7)
		     (offset nil)
		     (pos nil)
		     (draw t))
  (let ((bbox (bbox (or pos at-what))))
    (conform bbox :scale scale :position position :orientation orientation)
    (if draw (draw at-what :color color :clear t :offset offset :position pos))))


(defmethod draw-struct ((obj list) &key
			(complex nil)
			(color nil) (update t) (clear nil)
			(fill nil) (flash nil) (oriented nil)(radius nil)
			(viewport *viewport*) (scale 0.8)(boundary t))
  (if clear (clear-display))
  (loop for item in obj do
	(draw-struct item :fill fill
		     :oriented oriented
		     :flash flash :color color :update nil :clear nil
		     :viewport viewport :scale scale :boundary boundary
		     :radius radius))
  (if update (x-force-output)))

(defmethod draw-struct :around ((cell simplex) 
			&key
			(complex nil)
			(color *blue*) (update t)
			(boundary t)
			(clear nil)
			(viewport *viewport*)(scale 0.8)
			(oriented nil)
			(radius nil)
			(flash nil)
			(fill nil))
  (cond ((plusp (dimension-of cell))
	 (call-next-method))
	(t (and (null boundary)
		(< (dimension-of cell) (max-dimension-of complex))
		(null (rest (cofacets cell complex)))
		(return-from draw-struct nil))
	   (if flash
	       (flash cell :color color :clear clear)
	     (draw cell :color color :update update :clear clear)))))

(defun simplex-combinations (list k &optional (dim nil))
  (unless dim (setf dim (1- (length list))))
  (cond ((= k 0) (mapcar #'list list))
	((= k dim) (list list))
	((> k dim) nil)
	(t
	 (loop for l on list
	       with l2 = nil 
	       nconc
	       (loop for rest in
		     (simplex-combinations (rest l) k (1- dim)) collect
		     (append (reverse l2) rest))
	       do (push (first l) l2)))))

(defmethod draw-struct ((cell simplex) 
			&key
			(complex nil)
			(color *blue*) (update t)
			(boundary t)
			(clear nil)
			(viewport *viewport*)(scale 0.8)
			(oriented nil)
			(radius nil)
			(fill nil))
  (and (null boundary)
       (< (dimension-of cell) (max-dimension-of (complex-of cell)))
       (null (rest (cofacets cell complex)))
       (return-from draw-struct nil))
  
  (let* ((center (barycenter cell))
	 (coords (list scale (- 1 scale)))
	 (points (loop for v in (vertices-of cell) collect
		       (barycenter (list v center) coords)))
	 (edges (simplex-combinations points 1))
	 (faces (simplex-combinations points 2)))
    
    (if fill (loop for face in faces do
		   (draw face :color color)))
    (if (and fill (> (dimension-of cell) 1) )
	(loop for face in  (simplex-combinations points 2) do
	      (draw-cell cell face :color color)))
    (cond ((= (dimension-of cell) 1)
	   (loop for edge in  (simplex-combinations points 1) do
		 (draw-edge edge :color color :oriented oriented)))
	  (t (loop for edge in  (simplex-combinations points 1) do
		   (draw-edge edge :color color :oriented nil))
	     (when (and oriented (= (dimension-of cell) 2))
	       (draw-arrow-arc
		(lisp-number (barycenter cell))
		(or radius
		    (* 0.12
		       (loop with bb = (lbbox cell)
			     for min in (first bb)
			     for max in (second bb)
			     minimize (- max min))))
		:width 1
		:color (if fill *black* color)
		:end 5.0
		:ratio 0.3))))))

(defun draw-all (cm &key (clear nil)(scale 0.8)(oriented nil)
		    (color nil) (width 3)(radius nil)
		    (fill nil) (ratio 0.1)
		    (which-cells '(0 1 2 3))
		    (background t))
  (if clear (clear-display))
  (if background
      (let ((*line-width* 1))
	(declare (special *line-width*))
	(draw cm :color (or color *green*))))
  (setf *line-width* width)
  (loop for i upto (max-dimension-of cm)
	for c in (list *black* *red* *blue* *green*) do
	(if (member i which-cells)
	    (draw-struct (k-cells cm i)
			 :color (or color c)
			 :scale scale
			 :oriented oriented
			 :fill fill
			 :radius radius))))
