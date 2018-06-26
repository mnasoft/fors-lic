;;;; fors-lic.lisp

(in-package #:fors-lic)

;;; "fors-lic" goes here. Hacks and glory await!

(defclass ch nil
  ((mfr :accessor ch-mfr
	:initform 1.0
	:initarg  :mfr
	:documentation "Массовый расход в контрольной точке")
   (pdr :accessor ch-pdr
	:initform 3.0
	:initarg  :pdr
	:documentation "Перепад давления в контрольной точке")
   (den :accessor ch-den
	:initform 835.0
	:initarg  :den
	:documentation "Плотность жидкости в контрольной точке"))
  (:documentation "Ch представляет из себя канал форсунки, для которого выполняется соотношение:
G  = A*(Δp*ρ)^0.5, где
G  - расход через форсунку;
Α  = const;
Δp - перепад давления на канале форсунке;
ρ  - плотность жидкости, протекающая через канал форсунки "))

(export 'ch)
(export 'ch-mfr)
(export 'ch-pdr)
(export 'ch-den)

(defmethod print-object ((x ch) s)
  (format s "#ch(ch-mfr=~S ch-pdr=~S ch-den=~S)"
	  (ch-mfr x) (ch-pdr x) (ch-den x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric mass-flow-rate (chennel pressure-drop &key licuid-density)
  (:documentation "Функция определения массового расхода 
через канал форсунки chennel
при перепаде давления pressure-drop
и плотности рабочей среды licuid-density"))

(export 'mass-flow-rate)

(defmethod mass-flow-rate ((x ch) pressure-drop &key (licuid-density (ch-den x)))
  (* (ch-mfr x)
     (sqrt (/ (* pressure-drop licuid-density)
	      (* (ch-pdr x) (ch-den x))))))

(defgeneric pd (chennel mass-flow-rate &key licuid-density)
    (:documentation "Функция определения перепада давления 
на канале форсунки chennel
при массовом расходе через него mass-flow-rate
и плотности рабочей среды licuid-density"))

(defmethod pd ((x ch) mass-flow-rate &key (licuid-density (ch-den x)))
  (* (/ mass-flow-rate (ch-mfr x))
     (/ mass-flow-rate (ch-mfr x))
     (ch-pdr x)
     (/ (ch-den x)
	licuid-density)))

(export 'pd)

(format t (documentation 'ch 'type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass fors_2 nil
  ((ch1 :accessor fors_2-ch1
	:initform (make-instance 'ch :mfr 1200.0)
	:initarg  :ch1
	:documentation "Первый канал форсунки")
   (ch2 :accessor fors_2-ch2
	:initform (make-instance 'ch :mfr (- 7110.0 1200.0))
	:initarg  :ch2
	:documentation "Второй канал форсунки")
   (tbl :accessor fors_2-tbl
	:initform '((0.0 0.0)(0.7 0.0)(1.5 0.5)(3.0 3.0) (4.0 4.0))
	:initarg  :fors_2-tbl
	:documentation "Табличная зависимость перепада давления во втором канале форсунки от перепада давления в первом канале форсунки"))  
  (:documentation "fors_2 представляет из себя двухканальную форсунку, для которой выполняется соотношение:
GΣ = A1*(Δp1*ρ1)^0.5+A2*(Δp2*ρ2)^0.5, где
GΣ - расход через форсунку;
Α1 = const; Α2=const;
Δp1- перепад давления на первом канале форсунке;
ρ1 - плотность жидкости, протекающая через первый канал форсунки
Δp2- перепад давления на втором канале форсунке;
ρ2 - плотность жидкости, протекающая через второй канал форсунки"))

(export 'fors_2)
(export 'fors_2-ch1)
(export 'fors_2-ch2)
(export 'fors_2-tbl)

(defmethod print-object ((x fors_2) s)
  (format s "#fors_2(~%	~S~%	~S)"
	  (fors_2-ch1 x) (fors_2-ch2 x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod mass-flow-rate ((x fors_2) pressure-drop &key (licuid-density (ch-den (fors_2-ch1 x))))
  (+ (mass-flow-rate (fors_2-ch1 x) pressure-drop   :licuid-density licuid-density)
     (mass-flow-rate (fors_2-ch2 x) (pd2-by-pd1 x pressure-drop) :licuid-density licuid-density)))

(defmethod pd ((x fors_2) mass-flow-rate &key (licuid-density (ch-den (fors_2-ch1 x))))
  (half-div:H-DIV-LST 0 10.0
		      #'(lambda (pd G fors) (- G (mass-flow-rate fors pd )))
		      0 (list t mass-flow-rate x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric pd2-by-pd1 (fors pressure-drop-on-cannel1))

(export 'pd2-by-pd1)

(defgeneric pd1-by-mfr (fors mass-flow-rate &key licuid-density)
    (:documentation "Функция определения перепада давления 
на первом канале форсунки при массовом расходе через форсунку 
mass-flow-rate и плотности рабочей среды licuid-density"))

(export 'pd1-by-mfr)

(defgeneric pd2-by-mfr (fors mass-flow-rate &key licuid-density)
    (:documentation "Функция определения перепада давления 
на втором канале форсунки при массовом расходе через него
 mass-flow-rate и плотности рабочей среды licuid-density"))

(export 'pd2-by-mfr)

(defgeneric mfr1-by-mfr (fors mass-flow-rate &key licuid-density)
    (:documentation "Функция определения расхода через
первый канал форсунки при массовом расходе через форсунку
mass-flow-rate и плотности рабочей среды licuid-density"))

(export 'mfr1-by-mfr)

(defgeneric mfr2-by-mfr (fors mass-flow-rate &key licuid-density)
    (:documentation "Функция определения расхода через
второй канал форсунки при массовом расходе через форсунку
mass-flow-rate и плотности рабочей среды licuid-density"))

(export 'mfr2-by-mfr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod pd2-by-pd1 ((x fors_2) pressure-drop-on-cannel1)
  (math:appr_table pressure-drop-on-cannel1 (fors_2-tbl x)))

(defmethod pd1-by-mfr ((x fors_2) mass-flow-rate &key (licuid-density (ch-den (fors_2-ch1 x))))
  (pd x mass-flow-rate  :licuid-density licuid-density))

(defmethod pd2-by-mfr ((x fors_2) mass-flow-rate &key (licuid-density (ch-den (fors_2-ch1 x))))
    (pd2-by-pd1 x (pd x mass-flow-rate  :licuid-density licuid-density))
)

(defmethod mfr1-by-mfr ((x fors_2) mass-flow-rate &key (licuid-density (ch-den (fors_2-ch1 x))))
  (mfr (fors_2-ch1 x)
       (pd1-by-mfr x mass-flow-rate :licuid-density licuid-density)))

(defmethod mfr2-by-mfr ((x fors_2) mass-flow-rate &key licuid-density)
    (mass-flow-rate (fors_2-ch2 x)
		    (pd2-by-mfr x mass-flow-rate :licuid-density licuid-density)))
