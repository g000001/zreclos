;;;; readtable.lisp -*- Mode: Lisp;-*- 

(cl:in-package :zreclos.meta)


(defmacro in-syntax (readtable)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *readtable* ,readtable)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *zreclos-syntax* (copy-readtable nil))
  
  (in-syntax *zreclos-syntax*)
  
  (defun zreclos-prefix (srm chr)
    (declare (ignore chr))
    (let ((*package* (find-package 'zreclos)))
      (read srm)))

  (set-macro-character #\~ #'zreclos-prefix))


(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))


;;; *EOF*
