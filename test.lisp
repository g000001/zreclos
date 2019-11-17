;;;; test.lisp -*- Mode: Lisp;-*- 

(cl:in-package zreclos.meta)

(def-suite zreclos)
(in-suite zreclos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(zreclos:defclass a-class (standard-class) ())
(zreclos:defclass b-class (standard-class) ())
(zreclos:defclass c-class (a-class b-class) ())
(defmethod validate-superclass ((c a-class) (s standard-class)) T)
(defmethod validate-superclass ((c b-class) (s standard-class)) T)

(defconstant <a>
  (zreclos:defclass a ()
    ()
    (:metaclass a-class)))

(defconstant <b>
  (zreclos:defclass b ()
    ()
    (:metaclass b-class)))

(defconstant <c>
  (zreclos:defclass c (a b)
    ()))

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

#||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||;
(run!)
;||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||#

;;; *EOF*

