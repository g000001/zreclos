;;;; zreclos.lisp -*- Mode: Lisp;-*- 
(cl:in-package :zreclos.meta)
(in-syntax *zreclos-syntax*)

#|;



(defclass ~self-referent-operating-object 
          (~operating-object ~self-referent-object)
  ()
  (:metaclass ~self-referent-operating-class))


'(defclass ~lazy-class
          (~self-referent-operating-class)
  ()
  (:metaclass standard-class))


'(defclass ~attributed-class
          (~self-referent-operating-class)
  ()
  (:metaclass standard-class))


'(defclass ~attributed-lazy-class
          (~attributed-class
           ~lazy-class)
  ()
  (:metaclass standard-class))


'(defclass ~constrained-class
          (~attributed-class)
  ()
  (:metaclass standard-class))


'(defclass ~constrained-lazy-class
          (~constrained-class
           ~lazy-class)
  ()
  (:metaclass standard-class))


(defclass ~ir-operating-class
          (~instance-recording-class
           ~operating-class)
  ()
  (:metaclass standard-class))

(defclass ~ir-operating-object
          (~instance-recording-object
           ~operating-object)
  ()
  (:metaclass ~ir-operating-class))


(defclass ~ir-self-referent-operating-class
          (~instance-recording-class 
           ~self-referent-operating-class
           ~operating-class)
  ()
  (:metaclass standard-class))


(defclass ~ir-self-referent-operating-object
          (~instance-recording-object 
           ~self-referent-operating-object
           ~operating-object)
  ()
  (:metaclass ~ir-self-referent-operating-class))


'(defclass ~ir-lazy-class
          (~ir-self-referent-operating-class
           ~lazy-class)
  ()
  (:metaclass standard-class))


'(defclass ~ir-attributed-lazy-class
          (~ir-attributed-class
           ~attributed-lazy-class
           ~ir-lazy-class)
  ()
  (:metaclass standard-class))


'(defclass ~ir-attributed-class
          (~instance-recording-class
           ~attributed-class)
  ()
  (:metaclass standard-class))


'(defclass ~ir-constrained-class
          (~instance-recording-class
           ~constrained-class)
  ()
  (:metaclass standard-class))


'(defclass ~ir-constrained-lazy-class
          (~ir-constrained-class
           ~constrained-lazy-class
           ~ir-attributed-lazy-class)
  ()
  (:metaclass standard-class))

;|#
;;; *EOF*
