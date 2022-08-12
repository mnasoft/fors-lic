;;;; fors-lic.lisp

(defpackage #:fors-lic
  (:use #:cl)
  (:export pd2-by-pd1  pd2-by-pd1
	   pd1-by-mfr  pd2-by-mfr 
	   mfr1-by-mfr mfr2-by-mfr
	   mass-flow-rate)
  (:export <fors-2>
	   <fors-2>-channel-1
           <fors-2>-channel-2
           <fors-2>-tbl
           )
  (:export <channel>
           <channel>-pdr
           <channel>-mfr
           <channel>-den)
  (:export pd)
  (:export print-object)
  (:documentation
   " @b(Описание:) пакет @b(fors-lic) поределяет следующие классы:
@begin(list)
 @item(<channel> - канал подачи жидкого топлива;)
 @item(<fors-2>  - двухканльная форсунка подачи жидкого топлива.)
@end(list)

 @b(Примеры использования:)

 Задача 1

 Определить расход через канал форсунки, спроектированном на следующие параметры:
@begin(list)
 @item(расход 75 кг/ч; перепад давления 30 кгс/см@sup(2);)
 @item(рабочая жидкость имеет плотность 835 кг/м@sup(3);)
 @item(при перепаде давления на канале 20 кгс/см@sup(2).)
@end(list)

 @b(Решение:)
@begin[lang=lisp](code)
 (in-package #:fors-lic)
 (let ((ch (make-instance 'ch :mfr (* 75.0) :pdr 30.0 )))
  (mass-flow-rate ch 20.0))
 =>61.237247 [кг/ч]
 @end(code)

 Задача 2

 Определить перепад давления на канале форсунки, спроектированном на следующие параметры:
@begin(list)
 @item(расход 75 кг/ч; перепад давления 30 кгс/см@sub(2);)
 @item(рабочая жидкость имеет плотность 835 кг/м@sup(3);)
 @item(при расходе через канал 100 кг/ч.)
@end(list)

 @b(Решение:)
@begin[lang=lisp](code)
 (in-package #:fors-lic)
 (let ((ch (make-instance 'ch :mfr (* 75.0 ) :pdr 30.0 )))
       (pd ch 100.0))
 =>53.333336 [кгс/см2]
@end(code)

 Задача 3
Определить перепад давления на канале форсунки, спроектированном на следующие параметры:
@begin(list)
 @item(расход 75 кг/ч; перепад давления 30 кгс/см@sup(2);)
 @item(рабочая жидкость имеет плотность 835 кг/м@sup(3);)
 @item(при расходе через канал 100 кг/ч и работе на жидкости с полтностью 1000 кг/м@sup(3).)
@end(list)

 @b(Решение:)
@begin[lang=lisp](code)
 (in-package #:fors-lic)
 (let ((ch (make-instance 'ch :mfr (* 75.0 ) :pdr 30.0 )))
       (pd ch 100.0 :licuid-density 1000.0))
  =>44.533333 [кгс/см2]
@end(code)
"))

(in-package #:fors-lic)

(defclass <channel> ()
  ((mfr :accessor <channel>-mfr :initform 1.0   :initarg  :mfr :documentation "Массовый расход в контрольной точке")
   (pdr :accessor <channel>-pdr :initform 3.0   :initarg  :pdr :documentation "Перепад давления в контрольной точке")
   (den :accessor <channel>-den :initform 835.0 :initarg  :den :documentation "Плотность жидкости в контрольной точке"))
  (:documentation "<Channel> представляет из себя канал форсунки, для которого выполняется соотношение:
G  = A*(Δp*ρ)^0.5, где
G  - расход через форсунку;
Α  = const;
Δp - перепад давления на канале форсунке;
ρ  - плотность жидкости, протекающая через канал форсунки "))

(defmethod print-object ((x <channel>) s)
"@begin[lang=lisp](code)
 (defparameter *<channel>* (make-instance '<channel> :mfr 340))
 (format t *<channel>*) ; => #<channel>(<channel>-mfr=340 <channel>-pdr=3.0 <channel>-den=835.0)
@end(code)
"
  (format s "#<channel>(<channel>-mfr=~S <channel>-pdr=~S <channel>-den=~S)"
	  (<channel>-mfr x) (<channel>-pdr x) (<channel>-den x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric mass-flow-rate (<channel> pressure-drop &key liquid-density)
  (:documentation "@b(Описание:) обобщенная функция @b(mass-flow-rate)
определения массового расхода через канал форсунки @b(<channel>) при 
перепаде давления @b(pressure-drop) и плотности 
рабочей среды @b(liquid-density)."))

(defgeneric pd (channel mass-flow-rate &key liquid-density)
  (:documentation
   " @b(Описание:) обобщенная функция @b(pd) определения перепада
 давления на канале форсунки @b(channel) при массовом расходе через
 него @b(mass-flow-rate) и плотности рабочей среды
 @(liquid-density)."))

(defmethod mass-flow-rate ((x <channel>) pressure-drop &key (liquid-density (<channel>-den x)))
"@b(Описание:) метод @b(mass-flow-rate) для класса @b(<channel>)."
  (* (<channel>-mfr x)
     (sqrt (/ (* pressure-drop liquid-density)
	      (* (<channel>-pdr x) (<channel>-den x))))))

(defmethod pd ((x <channel>) mass-flow-rate &key (liquid-density (<channel>-den x)))
"@b(Описание:) метод @b(pd) для класса @b(pd)."
  (* (/ mass-flow-rate (<channel>-mfr x))
     (/ mass-flow-rate (<channel>-mfr x))
     (<channel>-pdr x)
     (/ (<channel>-den x)
	liquid-density)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <fors-2> ()
  ((channel-1 :accessor <fors-2>-channel-1
	:initform (make-instance '<channel> :mfr 1200.0)
	:initarg  :channel-1
	:documentation "Первый канал форсунки")
   (channel-2 :accessor <fors-2>-channel-2
	:initform (make-instance '<channel> :mfr (- 7110.0 1200.0))
	:initarg  :channel-2
	:documentation "Второй канал форсунки")
   (tbl :accessor <fors-2>-tbl
	:initform '((0.0 0.0)(0.7 0.0)(1.5 0.5)(3.0 3.0) (4.0 4.0))
	:initarg  :tbl
	:documentation "Табличная зависимость перепада давления во втором канале форсунки от перепада давления в первом канале форсунки")) 
  (:documentation
   "@b(Описание:) класс @b(<fors-2>) представляет из себя
   двухканальную форсунку, для которой выполняется соотношение:
@begin[lang=lisp](code)
 GΣ  = A1*(Δp1*ρ1)^0.5+A2*(Δp2*ρ2)^0.5, где
 GΣ  - расход через форсунку;
 Α1  = const; 
 Α2  = const;
 Δp1 - перепад давления на первом канале форсунке;
 ρ1  - плотность жидкости, протекающая через первый канал форсунки;
 Δp2 - перепад давления на втором канале форсунке;
 ρ2  - плотность жидкости, протекающая через второй канал форсунки.
@end(code)
"))

(defmethod print-object ((x <fors-2>) s)
  (format s "#<fors-2>(~%	~S~%	~S)"
	  (<fors-2>-channel-1 x) (<fors-2>-channel-2 x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod mass-flow-rate ((x <fors-2>) pressure-drop &key (liquid-density (<channel>-den (<fors-2>-channel-1 x))))
 " @b(Описание:) метод @b(mass-flow-rate) возврвщвет массовый расход
 через форсунку @b(x) при перепаде давления @b(pressure-drop) и
 плотности рабочей среды @b(liquid-density)."
  (+ (mass-flow-rate (<fors-2>-channel-1 x) pressure-drop   :liquid-density liquid-density)
     (mass-flow-rate (<fors-2>-channel-2 x) (pd2-by-pd1 x pressure-drop) :liquid-density liquid-density)))

(defmethod pd ((x <fors-2>) mass-flow-rate &key (liquid-density (<channel>-den (<fors-2>-channel-1 x))))
 " @b(Описание:) метод @b(pd) возвращает перепад давления на форсунке
 <fors-2> при массовом расходе через нее @b(mass-flow-rate) и
 плотности рабочей среды @b(liquid-density)."
  (math/half-div:H-DIV-LST 0 10.0
		      #'(lambda (pd G fors) (- G (mass-flow-rate fors pd )))
		      0 (list t mass-flow-rate x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric pd2-by-pd1 (fors pressure-drop-on-channel-1)
  (:documentation
   " @b(Описание:) обобщенная функция @b(pd2-by-pd1) определения
 перепада давления на втором канале форсунки fors по перепаду давления
 на первом канале @(pressure-drop-on-channel-1)"))

(defgeneric pd1-by-mfr (fors mass-flow-rate &key liquid-density)
  (:documentation
   " @b(Описание:) обобщенная функция @b(pd1-by-mfr) определения
 перепада давления на первом канале форсунки при массовом расходе
 через форсунку @b(mass-flow-rate) и плотности рабочей среды
 @b(liquid-density)"))

(defgeneric pd2-by-mfr (fors mass-flow-rate &key liquid-density)
  (:documentation
   " @b(Описание:) обобщенная функция @b(pd2-by-mfr) определения
 перепада давления на втором канале форсунки при массовом расходе
 через него @b(mass-flow-rate) и плотности рабочей среды
 @b(liquid-density)."))

(defgeneric mfr1-by-mfr (fors mass-flow-rate &key liquid-density)
  (:documentation
   " @b(Описание:) обобщенная функция @b(mfr1-by-mfr) определения
 расхода через первый канал форсунки при массовом расходе через
 форсунку @b(mass-flow-rate) и плотности рабочей среды
 @b(liquid-density)"))

(defgeneric mfr2-by-mfr (fors mass-flow-rate &key liquid-density)
  (:documentation
   " @b(Описание:) обобщенная функция @b(mfr2-by-mfr) определения
 расхода через второй канал форсунки при массовом расходе через
 форсунку @b(mass-flow-rate) и плотности рабочей среды
 @b(liquid-density)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod pd2-by-pd1 ((x <fors-2>) pressure-drop-on-channel-1)
  " @b(Описание:) метод @b(pd2-by-pd1) возвращает перепад давления на
 втором канале форсунки fors по перепаду давления на первом канале
 @b(pressure-drop-on-channel-1)."
  (math/appr:appr-table pressure-drop-on-channel-1 (<fors-2>-tbl x)))

(defmethod pd1-by-mfr ((x <fors-2>) mass-flow-rate &key (liquid-density (<channel>-den (<fors-2>-channel-1 x))))
  " @b(Описание:) метод @b(pd1-by-mfr) возвращает перепад давления на
 первом канале форсунки при массовом расходе через форсунку
 @b(mass-flow-rate) и плотности рабочей среды @b(liquid-density)."
  (pd x mass-flow-rate  :liquid-density liquid-density))

(defmethod pd2-by-mfr ((x <fors-2>) mass-flow-rate &key (liquid-density (<channel>-den (<fors-2>-channel-1 x))))
  " @b(Описание:) метод @b(pd2-by-mfr) возвращает давления на втором
 канале форсунки при массовом расходе через него @b(mass-flow-rate) и
 плотности рабочей среды @b(liquid-density)."
  (pd2-by-pd1 x (pd x mass-flow-rate  :liquid-density liquid-density)))

(defmethod mfr1-by-mfr ((x <fors-2>) mass-flow-rate &key (liquid-density (<channel>-den (<fors-2>-channel-1 x))))
  " @b(Описание:) метод @b(pd2-by-mfr) возвращает массовый расход
 топлива через первый канал форсунки при массовом расходе через
 форсунку @b(mass-flow-rate) и плотности рабочей среды @b(liquid-density)."
  (mass-flow-rate (<fors-2>-channel-1 x)
		  (pd1-by-mfr x mass-flow-rate :liquid-density liquid-density)))

(defmethod mfr2-by-mfr ((x <fors-2>) mass-flow-rate &key liquid-density)
  "@b(Описание:) метод @b(mfr2-by-mfr) возврвщает массовый расход
 через второй канал форсунки при массовом расходе через форсунку
 @b(mass-flow-rate) и плотности рабочей среды @b(liquid-density)."
  (mass-flow-rate (<fors-2>-channel-2 x)
		  (pd2-by-mfr x mass-flow-rate :liquid-density liquid-density)))
