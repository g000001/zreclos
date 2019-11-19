;;;; package.lisp -*- Mode: Lisp;-*- 

(cl:in-package :cl-user)


(defpackage zreclos.meta
  (:use c2cl fiveam))


(cl:in-package zreclos.meta)


(defpackage :zreclos
  (:use)
  (:export
   defclass
   defclass-eclos
   defclass-stklos
   self)
  (:export
   operating-class
   self-referent-operating-class
   self-referent-operating-object
   lazy-class
   attributed-class
   attributed-lazy-class
   constrained-class
   constrained-lazy-class
   self-referent-class
   self-referent-object
   instance-recording-class
   instance-recording-object
   instance-recording-structure-class
   scan-direct-instances
   ir-operating-class
   ir-self-referent-operating-class
   ir-lazy-class
   ir-attributed-lazy-class
   ir-attributed-class
   ir-constrained-class
   ir-constrained-lazy-class))


;;; *EOF*
