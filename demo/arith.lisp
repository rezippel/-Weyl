(in-package 'weyl)

(defvar *number-types* '(integer ratio float bigfloat lisp:complex))
(defvar *numeric-types* '(weyli::rational-integer weyli::rational-number
                          weyli::floating-point-number weyli::complex-number))

(defun check-for-methods (op)
  (let* ((types (append *number-types* *numeric-types*))
         (gf (if (clos::generic-function-p op) op
                 (symbol-function op)))
         (methods (generic-function-methods gf))
         (missing-methods 0))
    (labels ((find-method (arg1 arg2)
               (loop for meth in methods
                     for specs = (method-specializers meth)
                     do (when (and (eql (class-name (first specs)) arg1)
                                   (eql (class-name (second specs)) arg2))
                          (return meth)))))
      (loop for x in types do
        (loop for y in types do
          (unless (find-method x y)
            (incf missing-methods)
            (print (list x y))))))
    missing-methods))
        

(defun make-test-list ()
  (list 1 1.0 1/2 (lisp:complex 1 2)
        (make-element (get-rational-integers) 1)
        (make-element (get-rational-numbers) 1)
        (make-element (get-rational-numbers) 1/2)
        (make-element (get-real-numbers) 1)
        (make-element (get-real-numbers) 1.0)
        (make-element (get-real-numbers) 1/2)
        (make-element (get-complex-numbers) 1)
        (make-element (get-complex-numbers) 1.2)
        (make-element (get-complex-numbers) 1/3)
        (make-element (get-complex-numbers) (lisp:complex 1 3))))

(defun arith-domain-of (x)
  (cond ((typep x 'number) "Num")
        ((typep x 'weyli::domain-element) (domain-of x))
        (t "")))

(defun test-plus ()
  (let ((l (make-test-list))
        ans)
    (loop for a in l do
      (loop for b in l do
        (format t "~%(+ ~S(~S) ~S(~S)) = "
                a (if (typep a 'number) 'num (domain-of a))
                b (if (typep b 'number) 'num (domain-of b)))
        (multiple-value-bind (q r) (lcl:ignore-errors (+ a b))
	  (if r (format t "Error!")
	    (format t "~S(~A) " q (arith-domain-of q)))
	  )))))

(defun test-quotient ()
  (let ((l (make-test-list))
        ans)
    (loop for a in l do
      (loop for b in l do
        (format t "~%(/ ~S(~S) ~S(~S)) = "
                a (if (typep a 'number) 'num (domain-of a))
                b (if (typep b 'number) 'num (domain-of b)))
	(multiple-value-bind (q r) (lcl:ignore-errors (/ a b))
	  (if r (format t "Error!")
	    (format t "~S(~A) " q (arith-domain-of q)))
	  )))))

(defun test-truncate ()
  (let ((l (make-test-list)))
    (loop for a in l do
	  (loop for b in l do
		(format t "~%(truncate ~S(~A) ~S(~A)) = "
			a (arith-domain-of a) b (arith-domain-of b))
		(multiple-value-bind (q r) (lcl:ignore-errors (truncate a b))
		  (format t "~S(~A) ~S(~A)"
			  q (arith-domain-of q) (and q r) (arith-domain-of r)))))))

                      
