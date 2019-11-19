;;;; zreclos.lisp -*- Mode: Lisp;-*- 
(cl:in-package :zreclos.meta)
(in-syntax *zreclos-syntax*)


(defclass ~operating-class (cl:standard-class)
  ()
  (:metaclass cl:standard-class))


'(defclass ~self-referent-operating-class
          (~operating-class
           ~self-referent-class)
  ()
  (:metaclass cl:standard-class))


'(defclass ~lazy-class
          (~self-referent-operating-class)
  ()
  (:metaclass cl:standard-class))


'(defclass ~attributed-class
          (~self-referent-operating-class)
  ()
  (:metaclass cl:standard-class))


'(defclass ~attributed-lazy-class
          (~attributed-class
           ~lazy-class)
  ()
  (:metaclass cl:standard-class))


'(defclass ~constrained-class
          (~self-referent-operating-class)
  ()
  (:metaclass cl:standard-class))


'(defclass ~constrained-lazy-class
          (~constrained-class
           ~lazy-class)
  ()
  (:metaclass cl:standard-class))


(defclass ~self-referent-class (cl:standard-class)
  ()
  (:metaclass cl:standard-class))


'(defclass ~instance-recording-structure-class (structure-class)
  ((instance-record :initform (make-weak-vector 0 :adjustable T :fill-pointer 0)
                    :accessor ~class-instance-record))
  (:metaclass cl:standard-class))


(defclass ~instance-recording-object (standard-object)
  ()
  (:metaclass ~instance-recording-class))


'(defclass ~ir-operating-class
          (~instance-recording-class
           ~operating-class)
  ()
  (:metaclass cl:standard-class))


'(defclass ~ir-self-referent-operating-class
          (~instance-recording-class 
           ~self-referent-operating-class)
  ()
  (:metaclass cl:standard-class))


'(defclass ~ir-lazy-class
          (~ir-self-referent-operating-class
           ~lazy-class)
  ()
  (:metaclass cl:standard-class))


'(defclass ~ir-attributed-lazy-class
          (~ir-lazy-class
           ~ir-attributed-class
           ~attributed-lazy-class)
  ()
  (:metaclass cl:standard-class))


'(defclass ~ir-attributed-class
          (~instance-recording-class
           ~attributed-class)
  ()
  (:metaclass cl:standard-class))


'(defclass ~ir-constrained-class
          (~instance-recording-class
           ~constrained-class)
  ()
  (:metaclass cl:standard-class))


'(defclass ~ir-constrained-lazy-class
          (~ir-constrained-class
           ~constrained-lazy-class
           ~ir-attributed-lazy-class)
  ()
  (:metaclass cl:standard-class))


;;; *EOF*
