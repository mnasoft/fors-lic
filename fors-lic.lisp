;;;; fors-lic.lisp

(in-package #:fors-lic)

;;; "fors-lic" goes here. Hacks and glory await!

(defclass channel nil
  ((mass-flow-rate-design-point :accessor mfr-dp
				:initform 1.0
				:initarg  :mass-flow-rate-design-point
				:documentation "Массовый расход в контрольной точке")
   (pressure-drop-design-point :accessor pd-dp
			  :initform 3.0
			  :initarg  :pressure-design-point
				:documentation "Перепад давления в контрольной точке")
   (licuid-density-design-point :accessor ld-dp
				:initform 835.0
				:initarg  :licuid-density-design-point
				:documentation "Плотность жидкости в контрольной точке"))
  (:documentation "Channel представляет из себя канал форсунки, для которого выполняется соотношение:
G=A*(Δp*ρ)^0.5, где
G - расход через форсунку;
Α=const;
Δp - перепад давления на канале форсунке;
ρ  - плотность жидкости, протекающая через канал форсунки
Примеры использования:
1 Задача:
Определить расход через канал форсунки, спроектированном на следующие параметры:
- расход 75 кг/ч;
- перепад давления 30 кгс/см2;
- рабочая жидкость имеет плотность 835 кг/м3;
при перепаде давления на канале 20 кгс/см2
Решение:
(in-package #:fors-lic)
(let ((ch (make-instance 'channel
			 :mass-flow-rate-design-point (* 75.0 )
			 :pressure-design-point 30.0 )))
  (mass-flow-rate ch 20.0))
=>61.237247 [кг/ч]
2 Задача:
Определить перепад давления на канале форсунки, спроектированном на следующие параметры:
- расход 75 кг/ч;
- перепад давления 30 кгс/см2;
- рабочая жидкость имеет плотность 835 кг/м3;
при расходе через канал 100 кг/ч
Решение:
(in-package #:fors-lic)
(let ((ch (make-instance 'channel
			 :mass-flow-rate-design-point (* 75.0 )
			 :pressure-design-point 30.0 )))
  (pd ch 100.0))
=>53.333336 [кгс/см2]
"))

(defmethod print-object ((x channel) s)
  (format s "#channel(mfr-dp=~S pd-dp=~S ld-dp=~S)"
	  (mfr-dp x) (pd-dp x) (ld-dp x)))

(defgeneric mass-flow-rate (chennel pressure-drop &key licuid-density)
  (:documentation "Функция определения массового расхода 
через канал форсунки chennel
при перепаде давления pressure-drop
и плотности рабочей среды licuid-density"))

(defmethod mass-flow-rate ((x channel) pressure-drop &key (licuid-density (ld-dp x)))
  (* (mfr-dp x)
     (sqrt (/ (* pressure-drop licuid-density)
	      (* (pd-dp x) (ld-dp x))))))

(defgeneric pd (chennel mass-flow-rate &key licuid-density)
    (:documentation "Функция определения перепада давления 
на канале форсунки chennel
при массовом расходе через него mass-flow-rate
и плотности рабочей среды licuid-density"))

(defmethod pd ((x channel) mass-flow-rate &key (licuid-density (ld-dp x)))
  (* (/ mass-flow-rate (mfr-dp x))
     (/ mass-flow-rate (mfr-dp x))
     (pd-dp x)
     (/ (ld-dp x)
	licuid-density)))

(format t (documentation 'channel 'type))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass fors-lic nil
  ((ch1 :accessor channel-1
	:initform (make-instance 'fors-lic:channel :mass-flow-rate-design-point 1200.0)
	:initarg  :channel-1
	:documentation "Первый канал форсунки")
   (ch2 :accessor channel-2
	:initform (make-instance 'fors-lic:channel :mass-flow-rate-design-point (- 7110.0 1200.0))
	:initarg  :channel-2
	:documentation "Второй канал форсунки")
   (tbl-pd1-pd2 :accessor tbl-pd1-pd2
		:initform '((0.0 0.0)(0.7 0.0)(1.5 0.5)(3.0 3.0) (4.0 4.0))
		:initarg  :tbl-pd1-pd2
		:documentation "Табличная зависимость перепада давления во втором канале форсунки от перепада давления в первом канале форсунки"))  
  (:documentation "Fors-lic представляет из себя двухканальную форсунку, для которой выполняется соотношение:
GΣ=A1*(Δp1*ρ1)^0.5+A2*(Δp2*ρ2)^0.5, где
GΣ - расход через форсунку;
Α1=const; Α2=const;
Δp1 - перепад давления на первом канале форсунке;
ρ1  - плотность жидкости, протекающая через первый канал форсунки
Δp2 - перепад давления на втором канале форсунке;
ρ2  - плотность жидкости, протекающая через второй канал форсунки
Примеры использования:
1 Задача:
Определить расход через канал форсунки, спроектированном на следующие параметры:
- расход 75 кг/ч;
- перепад давления 30 кгс/см2;
- рабочая жидкость имеет плотность 835 кг/м3;
при перепаде давления на канале 20 кгс/см2
Решение
(let ((ch (make-instance 'channel
			 :mass-flow-rate-design-point (* 75.0 )
			 :pressure-design-point 30.0 )))
  (mass-flow-rate ch 20.0))
=>61.237247 [кг/ч]
2 Задача:
Определить перепад давления на канале форсунки, спроектированном на следующие параметры:
- расход 75 кг/ч;
- перепад давления 30 кгс/см2;
- рабочая жидкость имеет плотность 835 кг/м3;
при расходе через канал 100 кг/ч
(let ((ch (make-instance 'channel
			 :mass-flow-rate-design-point (* 75.0 )
			 :pressure-design-point 30.0 )))
  (pd ch 100.0))
=>53.333336 [кгс/см2]
"))

(defmethod print-object ((x fors-lic) s)
  (format s "#fors-lic(~%	~S~%	~S)"
	  (channel-1 x) (channel-2 x)))

(defmethod mass-flow-rate ((x fors-lic) pressure-drop &key (licuid-density (ld-dp (channel-1 x))))
  (+ (mass-flow-rate (channel-1 x) pressure-drop   :licuid-density licuid-density)
     (mass-flow-rate (channel-2 x) (pd2-by-pd1 x pressure-drop) :licuid-density licuid-density)))

(defmethod pd ((x fors-lic) mass-flow-rate &key (licuid-density (ld-dp (channel-1 x))))
  (half-div:H-DIV-LST 0 10.0
		      #'(lambda (pd G fors) (- G (mass-flow-rate fors pd )))
		      0 (list t mass-flow-rate x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric pd2-by-pd1 (fors pressure-drop-on-cannel1)
  )

(defgeneric pd1-by-mfr (fors mass-flow-rate &key licuid-density)
    (:documentation "Функция определения перепада давления 
на первом канале форсунки при массовом расходе через форсунку 
mass-flow-rate и плотности рабочей среды licuid-density"))

(defgeneric pd2-by-mfr (fors mass-flow-rate &key licuid-density)
    (:documentation "Функция определения перепада давления 
на втором канале форсунки при массовом расходе через него
 mass-flow-rate и плотности рабочей среды licuid-density"))

(defgeneric mfr1-by-mfr (fors mass-flow-rate &key licuid-density)
    (:documentation "Функция определения расхода через
первый канал форсунки при массовом расходе через форсунку
mass-flow-rate и плотности рабочей среды licuid-density"))

(defgeneric mfr2-by-mfr (fors mass-flow-rate &key licuid-density)
    (:documentation "Функция определения расхода через
второй канал форсунки при массовом расходе через форсунку
mass-flow-rate и плотности рабочей среды licuid-density"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod pd2-by-pd1 ((x fors-lic) pressure-drop-on-cannel1)
  (math:appr_table pressure-drop-on-cannel1 (tbl-pd1-pd2 x)))

(defmethod pd1-by-mfr ((x fors-lic) mass-flow-rate &key (licuid-density (ld-dp (channel-1 x))))
  (pd x mass-flow-rate  :licuid-density licuid-density))

(defmethod pd2-by-mfr ((x fors-lic) mass-flow-rate &key (licuid-density (ld-dp (channel-1 x))))
    (pd2-by-pd1 x (pd x mass-flow-rate  :licuid-density licuid-density))
)

(defmethod mfr1-by-mfr ((x fors-lic) mass-flow-rate &key (licuid-density (ld-dp (channel-1 x))))
  (mfr (channel-1 x)
       (pd1-by-mfr x mass-flow-rate :licuid-density licuid-density)))

(defmethod mfr2-by-mfr ((x fors-lic) mass-flow-rate &key licuid-density)
    (mass-flow-rate (channel-2 x)
	 (pd2-by-mfr x mass-flow-rate :licuid-density licuid-density)))





