;;; -*- Mode:Lisp; Package:Weyl; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

(in-package 'Weyl)

(defun fg (n)
  (let* ((z (get-rational-integers))
         (R (get-polynomial-ring z '(mu eps sigma)))
         (sigma (coerce 'sigma r))
         (mu (coerce 'mu r))
         (eps (coerce 'eps r))
         (x1 (* (- sigma) (+ mu (* 2 eps))))
         (x2 (+ eps (* -2 (expt sigma 2))))
         (x3 (* -3 mu sigma))
         (x4 (- mu))
         (f-array (make-array (1+ n) :initial-element nil))
         (g-array (make-array (1+ n) :initial-element nil)))
    (labels ((compute-f (n)
               (if (aref f-array n) (aref f-array n)
                   (setf (aref f-array n)
                         (+ (* x4 (compute-g (1- n)))
                            (* x1 (partial-deriv (compute-f (1- n)) eps))
                            (* x2 (partial-deriv (compute-f (1- n)) sigma))
                            (* x3 (partial-deriv (compute-f (1- n)) mu))))))
             (compute-g (n)
               (if (aref g-array n) (aref g-array n)
                   (setf (aref g-array n)
                         (+ (compute-f (1- n))
                            (* x1 (partial-deriv (compute-g (1- n)) eps))
                            (* x2 (partial-deriv (compute-g (1- n)) sigma))
                            (* x3 (partial-deriv (compute-g (1- n)) mu)))))))
      (setf (aref f-array 0) (coerce 0 r))
      (setf (aref g-array 0) (coerce 1 r))
      (values (compute-f n) (compute-g n)))))

#+Genera
(defun time-fg (n)
  (si:without-interrupts (time (fg n)))
  (values))


(defun time-fg (n)
  (time (fg n))
  (values))

#+Genera
(defun time-fg (n)
  (meter:with-monitoring t
    (let ((time (sys:%microsecond-clock)))
      (fg n)
      (/ (- (sys:%microsecond-clock) time) 1.0e6))))

;; The following times are for a straight Lisp algebra package (no
;; CLOS).  Its comparable to Macsyma's times for this computation.

;; FG     Lucid/4       MacIvory        Turbo/Mac
;; 5
;; 10
;; 15      1.62           5.4            2.9
;; 20      3.55          11.1            6.2
;; 25      6.60          20.8           11.7


;; These times were done in Lucid on a Spacstation.  Weyl/Opt uses a
;; special "polynomials over the integers" domain (zpolynomial).


;;               Sparc-1            Sparc-20
;; FG     Weyl          Weyl/Opt      Weyl
;; 5        1.6           1.1         0.03
;; 10       8.4           2.0         0.08
;; 15      28.0           4.2         0.27
;; 20      65.4           8.0         0.63
;; 25                    13.7         1.20
;; 30                                 2.11

;; These times are for Lucid 4.0 on a DECStation

;; FG       Weyl        Weyl/Opt
;;  5       0.14          0.08
;; 10       0.70          0.19
;; 15       2.53          0.48
;; 20       6.04          0.95
;; 25      11.65          1.78
