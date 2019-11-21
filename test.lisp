;;;; test.lisp -*- Mode: Lisp;-*- 

(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(def-suite zreclos)


(in-suite zreclos)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:defclass a-class (standard-class)
  ()
  (:metaclass standard-class))


(cl:defclass b-class (standard-class)
  ()
  (:metaclass standard-class))


(defmethod validate-superclass ((c a-class) (s standard-class)) T)


(defmethod validate-superclass ((c b-class) (s standard-class)) T)


(cl:defclass c-class (a-class b-class)
  ()
  (:metaclass standard-class))


(defmethod validate-superclass ((c c-class) (s standard-class)) T)


(defmethod validate-superclass ((c c-class) (s standard-class)) T)


(defconstant <a>
  (~defclass a ()
    ()
    (:metaclass a-class)))


(defconstant <b>
  (~defclass b ()
    ()
    (:metaclass b-class)))


(defconstant <c>
  (~defclass c (a b)
    ()
    (:metaclass c-class))) ;TODO


(test |compute-metaclass test|
  (defparameter *z* (gensym "z"))
  (defparameter *y* (gensym "y"))
  (ensure-class *z* :direct-superclasses (list *y*))
  (is (eq (compute-metaclass '())
          (find-class 'standard-class)))
  (is (eq (compute-metaclass '() :default-metaclass-name 'standard-class)
          (find-class 'standard-class)))
  (is (eq (compute-metaclass (list <a> <b>))
          (find-class 'c-class)))
  (is (eq (class-of <c>)
          (find-class 'c-class)))
  (is (eq (class-of (find-class *y*))
          (find-class 'forward-referenced-class)))
  (is (eq (class-of (ensure-class *y*))
          (find-class 'cl:standard-class))))


(test |self-referent-class test|
  (defun make-point (x y)
    (list x y))
  (~defclass (horizontal-line :stklos) (~self-referent-object)
    ((x1 :accessor x1 :initarg :x1 :type real)
     (x2 :accessor x2 :initarg :x2 :type real)
     (y :accessor y :initarg :y :type real)
     (point1 :initform (make-point (x1 ~self)
                                   (y ~self)))
     (point2 :initform (make-point (x2 ~self)
                                   (y ~self)))))
  (let ((obj (make-instance 'horizontal-line :x1 1 :x2 2 :y 3)))
    (is (equal 1 (slot-value obj 'x1)))
    (is (equal 2 (slot-value obj 'x2)))
    (is (equal '(1 3) (slot-value obj 'point1)))
    (is (equal '(2 3) (slot-value obj 'point2)))))


(test |instance-recording-class test|
  (~defclass irobj (~instance-recording-object) ())
  (~reset-instance-record (find-class 'irobj))
  (let ((size 100))
    (dotimes (i size)
      (make-instance 'irobj))
    (is (= size
           (series:collect-length
            (~scan-direct-instances
             (find-class 'irobj)))))))


#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||;
(run!)
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#


;;; *EOF*

