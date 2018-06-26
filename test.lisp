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

(require :ltk)
