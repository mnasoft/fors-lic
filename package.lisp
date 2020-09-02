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
	   mass-flow-rate)
  (:export fors_2
	   fors_2-channel1
	   channel-pdr pd
	   channel-mfr
	   channel-den
	   fors_2-channel2
	   channel
	   fors_2-tbl
	   print-object))

;;;;(declaim (optimize (space 0) (compilation-speed 0)  (speed 0) (safety 3) (debug 3)))
