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
   defclass-stklos))


;;; *EOF*
