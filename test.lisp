;;;; fors-lic.lisp

(in-package #:fors-lic)

(make-instance 'fors_2
	       :ch1 (make-instance 'ch :mfr 75.0)
	       :ch2 (make-instance 'ch :mfr (- 515.0 75.0)))

(mass-flow-rate *f* 3.0)

(pd1-by-mfr *f* 200.0)

(pd2-by-mfr *f* 200.0)

(+ (mass-flow-rate (fors_2-ch1 *f*) 1.2672263)
   (mass-flow-rate (fors_2-ch2 *f*) 0.35451648))


(defparameter *Г80038088*
  (make-instance 'fors_2
	       :ch1 (make-instance 'ch :mfr (/ (+ 75.0 80.0) 2.0 ))
	       :ch2 (make-instance 'ch :mfr (- (/ (+ 352 376) 2) 75.0))
	       :fors_2-tbl '((0.0 0.0)(0.7 0.0)(1.5 0.5)(3.0 2.0) (4.0 3.0)) ))

(defparameter Gt_1.0 (/ 5884.0 16.0))

(pd1-by-mfr *Г80038088* Gt_1.0)
(pd2-by-mfr *Г80038088* Gt_1.0)

(+ (mfr1-by-mfr *Г80038088* Gt_1.0 ) (mfr2-by-mfr *Г80038088* Gt_1.0 ))

(pd (fors_2-ch1 *Г80038088*) 26.9)

(* 3.0 (/ 26.9 77.5) (/ 26.9 77.5))
0.3614285

(require :ltk)


