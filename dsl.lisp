;;;; readtable.lisp -*- Mode: Lisp;-*- 

(cl:in-package :zreclos.meta)

(defmacro comment (&body body)
  (declare (ignore body))
  '(values))

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


(defun package-symbolconc (package-spec &rest frobs)
  (values
   (intern
    (with-standard-io-syntax
      (with-output-to-string (out)
        (dolist (elt frobs)
          (unless (typep elt '(or symbol string fixnum character))
            (error "The value ~A is not of type (OR SYMBOL STRING FIXNUM CHARACTER)."
                   elt))
          (let ((*print-base* 10.))
            (princ elt out)))))
    package-spec)))


(defun symbolconc (&rest frobs)
  (declare (dynamic-extent frobs))
  (let ((*package* (if (and (not (null (car frobs)))
                            (symbolp (car frobs)))
                       (symbol-package (car frobs))
                       *package*)))
  (apply #'package-symbolconc *package* frobs)))

(defun remove-\"class\"-suffix (sym)
 (let ((name (string sym)))
   (intern (subseq name 0 (search #.(string '-class) name))
           (symbol-package sym))))



