;;;; fors-lic.lisp

(in-package #:fors-lic)

;;; "fors-lic" goes here. Hacks and glory await!

(defclass channel nil
  ((mass-flow-rate-design-point :accessor mfr-dp
				:initform 1.0
				:initarg  :mass-flow-rate-design-point
				:documentation "Массовый расход в контрольной точке.")
   (pressure-drop-design-point :accessor pd-dp
			  :initform 3.0
			  :initarg  :pressure-design-point
				:documentation "Перепад давления в контрольной точке.")
   (licuid-density-design-point :accessor ld-dp
				:initform 835.0
				:initarg  :licuid-density-design-point
				:documentation "Плотность жидкости в контрольной точке."))
  (:documentation "Channel представляет из себя канал форсунки, для которого выполняется соотношение:
G=A*(Δp*ρ)^0.5, где
G - расход через форсунку;
Α=const;
Δp - перепад давления на канале форсунке;
ρ  - плотность жидкости, протекающая через канал форсунки.
Примеры использования:
1 Задача:
Определить расход через канал форсунки, спроектированном на следующие параметры:
- расход 75 кг/ч;
- перепад давления 30 кгс/см2;
- рабочая жидкость имеет плотность 835 кг/м3;
при перепаде давления на канале 20 кгс/см2.
Решение
(let ((ch (make-instance 'channel
			 :mass-flow-rate-design-point (* 75.0 )
			 :pressure-design-point 30.0 )))
  (mfr ch 20.0))
=>61.237247 [кг/ч]
2 Задача:
Определить перепад давления на канале форсунки, спроектированном на следующие параметры:
- расход 75 кг/ч;
- перепад давления 30 кгс/см2;
- рабочая жидкость имеет плотность 835 кг/м3;
при расходе через канал 100 кг/ч.
(let ((ch (make-instance 'channel
			 :mass-flow-rate-design-point (* 75.0 )
			 :pressure-design-point 30.0 )))
  (pd ch 100.0))
=>53.333336 [кгс/см2]
"))

(defmethod print-object ((x channel) s)
  (format s "#channel(mfr-dp=~S pd-dp=~S ld-dp=~S)"
	  (mfr-dp x) (pd-dp x) (ld-dp x)))

(defgeneric mfr (chennel pressure-drop &key licuid-density)
  (:documentation "Функция определения массового расхода 
через канал форсунки chennel
при перепаде давления pressure-drop
и плотности рабочей среды licuid-density."))

(defmethod mfr ((x channel) pressure-drop &key (licuid-density (ld-dp x)))
  (* (mfr-dp x)
     (sqrt (/ (* pressure-drop licuid-density)
	      (* (pd-dp x) (ld-dp x))))))

(defgeneric pd (chennel mass-flow-rate &key licuid-density)
    (:documentation "Функция определения перепада давления 
на канале форсунки chennel
при массовом расходе через него mass-flow-rate
и плотности рабочей среды licuid-density."))

(defmethod pd ((x channel) mass-flow-rate &key (licuid-density (ld-dp x)))
  (* (/ mass-flow-rate (mfr-dp x))
     (/ mass-flow-rate (mfr-dp x))
     (pd-dp x)
     (/ (ld-dp x)
	licuid-density)))

(format t (documentation 'channel 'type))
