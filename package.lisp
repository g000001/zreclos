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
   defmetaclass
   define-metaclass
   ultimate-ancestor-object-class-given-metaclass
   self)
  (:export
   operating-class
   operating-object
   self-referent-operating-class
   self-referent-operating-object
   lazy-class
   lazy-object
   attributed-class
   attributed-object
   attributed-lazy-class
   attributed-lazy-object
   constrained-class
   constrained-object
   constrained-lazy-class
   constrained-lazy-object
   self-referent-class
   self-referent-object
   instance-recording-class
   instance-recording-object
   instance-recording-structure-class
   instance-recording-structure-object
   ir-operating-class
   ir-operating-object
   ir-self-referent-operating-class
   ir-self-referent-operating-object
   ir-lazy-class
   ir-lazy-object
   ir-attributed-lazy-class
   ir-attributed-lazy-object
   ir-attributed-class
   ir-attributed-object
   ir-constrained-class
   ir-constrained-object
   ir-constrained-lazy-class
   ir-constrained-lazy-object)
  (:export 
   scan-direct-instances
   reset-instance-record
   mapslots
   mapslots*
   walkslots
   walkslots*)
  (:export 
   accessor-prefix-class
   accessor-prefix-object)
  (:export 
   attribute-value)
  (:export
   required-slot-class
   direct-required-slots
   class-required-slots))


(defpackage zreclos-user
  (:use c2cl zreclos)
  (:shadowing-import-from zreclos defclass))


;;; *EOF*
