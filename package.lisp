;;;; package.lisp

(defpackage #:fors-lic
  (:use #:cl)
  (:export fors-lic
	   pd2-by-pd1
	   pd1-by-mfr
	   pd2-by-mfr
	   mfr1-by-mfr
	   mfr2-by-mfr
	   pd2-by-pd1
	   pd1-by-mfr
	   pd2-by-mfr
	   mfr1-by-mfr
	   mfr2-by-mfr
	   mass-flow-rate))

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))





;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
