;;;; fors-lic.lisp

(in-package #:fors-lic)

(export 'channel)

(export 'channel-mfr)

(export 'channel-pdr)

(export 'channel-den)

(defclass channel nil
  ((mfr :accessor channel-mfr
	:initform 1.0
	:initarg  :mfr
	:documentation "Массовый расход в контрольной точке")
   (pdr :accessor channel-pdr
	:initform 3.0
	:initarg  :pdr
	:documentation "Перепад давления в контрольной точке")
   (den :accessor channel-den
	:initform 835.0
	:initarg  :den
	:documentation "Плотность жидкости в контрольной точке"))
  (:documentation "Channel представляет из себя канал форсунки, для которого выполняется соотношение:
G  = A*(Δp*ρ)^0.5, где
G  - расход через форсунку;
Α  = const;
Δp - перепад давления на канале форсунке;
ρ  - плотность жидкости, протекающая через канал форсунки "))

(export 'print-object )
(defmethod print-object ((x channel) s)
"@begin[lang=lisp](code)
 (defparameter *channel* (make-instance 'channel :mfr 340))
 (format t *channel*) ; => #channel(channel-mfr=340 channel-pdr=3.0 channel-den=835.0)
@end(code)
"
  (format s "#channel(channel-mfr=~S channel-pdr=~S channel-den=~S)"
	  (channel-mfr x) (channel-pdr x) (channel-den x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(mass-flow-rate))

(defgeneric mass-flow-rate (channel pressure-drop &key liquid-density)
  (:documentation "@b(Описание:) обобщенная функция @b(mass-flow-rate)
определения массового расхода через канал форсунки @b(channel) при 
перепаде давления @b(pressure-drop) и плотности 
рабочей среды @b(liquid-density)."))

(export '(pd))

(defgeneric pd (channel mass-flow-rate &key liquid-density)
    (:documentation "@b(Описание:) обобщенная функция @b(pd) определения 
перепада давления на канале форсунки @b(channel) при массовом расходе
через него @(mass-flow-rate) и плотности рабочей среды @(liquid-density)."))

(export '(mass-flow-rate))

(defmethod mass-flow-rate ((x channel) pressure-drop &key (liquid-density (channel-den x)))
"@b(Описание:) метод @b(mass-flow-rate) для класса @b(channel)."
  (* (channel-mfr x)
     (sqrt (/ (* pressure-drop liquid-density)
	      (* (channel-pdr x) (channel-den x))))))

(export '(pd))

(defmethod pd ((x channel) mass-flow-rate &key (liquid-density (channel-den x)))
"@b(Описание:) метод @b(pd) для класса @b(pd)."
  (* (/ mass-flow-rate (channel-mfr x))
     (/ mass-flow-rate (channel-mfr x))
     (channel-pdr x)
     (/ (channel-den x)
	liquid-density)))

;;;;(format t (documentation 'channel 'type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(fors_2 fors_2-channel1 fors_2-channel2 fors_2-tbl))

(defclass fors_2 ()
  ((channel1 :accessor fors_2-channel1
	:initform (make-instance 'channel :mfr 1200.0)
	:initarg  :channel1
	:documentation "Первый канал форсунки")
   (channel2 :accessor fors_2-channel2
	:initform (make-instance 'channel :mfr (- 7110.0 1200.0))
	:initarg  :channel2
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

(defmethod print-object ((x fors_2) s)
  (format s "#fors_2(~%	~S~%	~S)"
	  (fors_2-channel1 x) (fors_2-channel2 x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(mass-flow-rate))

(defmethod mass-flow-rate ((x fors_2) pressure-drop &key (liquid-density (channel-den (fors_2-channel1 x))))
"Функция определения массового расхода 
через форсунку fors_2
при перепаде давления pressure-drop
и плотности рабочей среды liquid-density"
  (+ (mass-flow-rate (fors_2-channel1 x) pressure-drop   :liquid-density liquid-density)
     (mass-flow-rate (fors_2-channel2 x) (pd2-by-pd1 x pressure-drop) :liquid-density liquid-density)))

(export 'pd)

(defmethod pd ((x fors_2) mass-flow-rate &key (liquid-density (channel-den (fors_2-channel1 x))))
"Функция определения перепада давления 
на форсунке fors_2 при массовом расходе через него mass-flow-rate
и плотности рабочей среды liquid-density"
  (half-div:H-DIV-LST 0 10.0
		      #'(lambda (pd G fors) (- G (mass-flow-rate fors pd )))
		      0 (list t mass-flow-rate x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(pd2-by-pd1))

(defgeneric pd2-by-pd1 (fors pressure-drop-on-channel1)
  (:documentation "@b(Описание:) обобщенная функция @b(pd2-by-pd1)
определения перепада давления на втором канале форсунки fors по 
перепаду давления на первом канале @(pressure-drop-on-channel1)"))

(export '(pd1-by-mfr))

(defgeneric pd1-by-mfr (fors mass-flow-rate &key liquid-density)
    (:documentation "@b(Описание:) обобщенная функция @b(pd1-by-mfr) определения
перепада давления на первом канале форсунки при массовом расходе 
через форсунку @b(mass-flow-rate) и плотности рабочей среды @b(liquid-density)"))

(export '(pd2-by-mfr))

(defgeneric pd2-by-mfr (fors mass-flow-rate &key liquid-density)
  (:documentation "@b(Описание:) обобщенная функция @b(pd2-by-mfr) определения перепада
давления на втором канале форсунки при массовом расходе через него
@b(mass-flow-rate) и плотности рабочей среды @b(liquid-density)."))

(export 'mfr1-by-mfr )

(defgeneric mfr1-by-mfr (fors mass-flow-rate &key liquid-density)
    (:documentation "@b(Описание:) обобщенная функция @b(mfr1-by-mfr)
определения расхода через первый канал форсунки при массовом 
расходе через форсунку @b(mass-flow-rate) и плотности 
рабочей среды @b(liquid-density)"))




(defgeneric mfr2-by-mfr (fors mass-flow-rate &key liquid-density)
    (:documentation "@b(Описание:) обобщенная функция @b(mfr2-by-mfr) определения 
расхода через второй канал форсунки при массовом расходе через форсунку
@b(mass-flow-rate) и плотности рабочей среды @b(liquid-density)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export 'pd2-by-pd1 )

(defmethod pd2-by-pd1 ((x fors_2) pressure-drop-on-channel1)
"Функция определения перепада давления 
на втором канале форсунки fors по перепаду давления на первом канале
pressure-drop-on-channel1"
  (math/appr:appr-table pressure-drop-on-channel1 (fors_2-tbl x)))

(export 'pd1-by-mfr )

(defmethod pd1-by-mfr ((x fors_2) mass-flow-rate &key (liquid-density (channel-den (fors_2-channel1 x))))
"Функция определения перепада давления 
на первом канале форсунки при массовом расходе через форсунку 
mass-flow-rate и плотности рабочей среды liquid-density"
  (pd x mass-flow-rate  :liquid-density liquid-density))

(export 'pd2-by-mfr )

(defmethod pd2-by-mfr ((x fors_2) mass-flow-rate &key (liquid-density (channel-den (fors_2-channel1 x))))
"Функция определения перепада давления на втором канале форсунки
при массовом расходе через него mass-flow-rate и плотности 
рабочей среды liquid-density"
    (pd2-by-pd1 x (pd x mass-flow-rate  :liquid-density liquid-density))
  )

(export 'mfr1-by-mfr )

(defmethod mfr1-by-mfr ((x fors_2) mass-flow-rate &key (liquid-density (channel-den (fors_2-channel1 x))))
"Функция определения расхода через
первый канал форсунки при массовом расходе через форсунку
mass-flow-rate и плотности рабочей среды liquid-density"
  (mass-flow-rate (fors_2-channel1 x)
		  (pd1-by-mfr x mass-flow-rate :liquid-density liquid-density)))

(export 'mfr2-by-mfr )

(defmethod mfr2-by-mfr ((x fors_2) mass-flow-rate &key liquid-density)
"Функция определения расхода через
второй канал форсунки при массовом расходе через форсунку
mass-flow-rate и плотности рабочей среды liquid-density"
    (mass-flow-rate (fors_2-channel2 x)
		    (pd2-by-mfr x mass-flow-rate :liquid-density liquid-density)))
