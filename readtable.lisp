;;;; readtable.lisp -*- Mode: Lisp;-*- 

(cl:in-package :zreclos.internal)
(in-readtable :common-lisp)


(defreadtable :zreclos
  (:merge :standard)
  (:macro-char char fctn opt...)
  (:syntax-from readtable to-char from-char)
  (:case :upcase))


;;; *EOF*
