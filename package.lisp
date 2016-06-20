;;;; package.lisp

(defpackage #:fors-lic
  (:use #:cl)
  (:export channel
	   fors-lic
	   )
  (:export mass-flow-rate
	   pd
	   mfr1-by-mfr
	   mfr2-by-mfr
	   pd1-by-mfr
	   pd2-by-mfr
   	   pd2-by-pd1
	   )
  )

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))
