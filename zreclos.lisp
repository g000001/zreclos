;;;; zreclos.lisp -*- Mode: Lisp;-*- 

(cl:in-package :zreclos.meta)


(defclass zreclos:operating-class (standard-class)
  ()
  (:metaclass standard-class))


(defclass zreclos:self-referent-operating-class
          (zreclos:operating-class
           zreclos:self-referent-class)
  ()
  (:metaclass standard-class))


(defclass zreclos:lazy-class
          (zreclos:self-referent-operating-class)
  ()
  (:metaclass standard-class))


(defclass zreclos:attributed-class
          (zreclos:self-referent-operating-class)
  ()
  (:metaclass standard-class))


(defclass zreclos:attributed-lazy-class
          (zreclos:attributed-class
           zreclos:lazy-class)
  ()
  (:metaclass standard-class))


(defclass zreclos:constrained-class
          (zreclos:self-referent-operating-class)
  ()
  (:metaclass standard-class))


(defclass zreclos:constrained-lazy-class
          (zreclos:constrained-class
           zreclos:lazy-class)
  ()
  (:metaclass standard-class))


(defclass zreclos:self-referent-class (standard-class)
  ()
  (:metaclass standard-class))


(defclass zreclos:instance-recording-class (standard-class)
  ()
  (:metaclass standard-class))


(defclass zreclos:ir-operating-class
          (zreclos:instance-recording-class
           zreclos:operating-class)
  ()
  (:metaclass standard-class))


(defclass zreclos:ir-self-referent-operating-class
          (zreclos:instance-recording-class 
           zreclos:self-referent-operating-class)
  ()
  (:metaclass standard-class))


(defclass zreclos:ir-lazy-class
          (zreclos:ir-self-referent-operating-class
           zreclos:lazy-class)
  ()
  (:metaclass standard-class))


(defclass zreclos:ir-attributed-lazy-class
          (zreclos:ir-lazy-class
           zreclos:ir-attributed-class
           zreclos:attributed-lazy-class)
  ()
  (:metaclass standard-class))


(defclass zreclos:ir-attributed-class
          (zreclos:instance-recording-class
           zreclos:attributed-class)
  ()
  (:metaclass standard-class))


(defclass zreclos:ir-constrained-class
          (zreclos:instance-recording-class
           zreclos:constrained-class)
  ()
  (:metaclass standard-class))


(defclass zreclos:ir-constrained-lazy-class
          (zreclos:ir-constrained-class
           zreclos:constrained-lazy-class
           zreclos:ir-attributed-lazy-class)
  ()
  (:metaclass standard-class))


;;; *EOF*
