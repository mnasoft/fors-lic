;;;; fors-lic.lisp

(in-package #:fors-lic)

(make-instance 'fors-lic:channel :mass-flow-rate-design-point 75.)

(fors-lic:mass-flow-rate *ch*   )
