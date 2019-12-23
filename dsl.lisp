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

(defgeneric ultimate-ancestor-object-class-given-metaclass (class))

(defmethod ultimate-ancestor-object-class-given-metaclass ((class (eql 'standard-class)))
  'standard-object)

(defmacro defmetaclass (name (&rest supers) (&rest slots) &rest opts)
  (let* ((prefix (remove-\"class\"-suffix name))
         (default-supers (or supers '(standard-class)))
         (defeslotds (find :effective-slot-definitions-mixin-slots
                           opts :key #'car))
         (defdslotds (find :direct-slot-definitions-mixin-slots opts
                           :key #'car))
         (defslotds (find :slot-definitions-mixin-slots opts
                          :key #'car))
         (newopts (mapcan (lambda (o)
                            (case (car o)
                              ((:effective-slot-definitions-mixin-slots
                                :direct-slot-definitions-mixin-slots
                                :slot-definitions-mixin-slots)
                               nil)
                              (otherwise (list o))))
                          opts))
         (slotd  (symbolconc prefix '-slot-definition))
         (dslotd (symbolconc prefix '-direct-slot-definition))
         (eslotd (symbolconc prefix '-effective-slot-definition)))
    `(progn
       (finalize-inheritance
        (~defclass ,name (,@default-supers)
          ,slots
          ,@newopts))
       ,@(mapcar (lambda (s)
                   `(defmethod validate-superclass ((class ,name)
                                                    (super ,s))
                      T))
                 default-supers)
       (defmethod ultimate-ancestor-object-class-given-metaclass 
                  ((class (eql ',name)))
         (find-class ',(symbolconc prefix '-object)))
       (finalize-inheritance
        (ensure-class ',(symbolconc prefix '-object)
                      :direct-superclasses 
                      (mapcar #'ultimate-ancestor-object-class-given-metaclass ',default-supers)
                      :metaclass ',name))
       ,@(and 
          defslotds
          `((finalize-inheritance
             (defclass ,slotd (standard-slot-definition) 
               ,@(and defslotds (list (cdr defslotds)))))))
       ,@(and 
          defdslotds
          `((finalize-inheritance
             (defclass ,dslotd (standard-direct-slot-definition ,slotd) 
               ,@(and defdslotds (list (cdr defdslotds)))))
            (defmethod direct-slot-definition-class ((class ,name)
                                                     &rest initargs)
              (declare (ignore initargs))
              (find-class ',dslotd))))
       ,@(and
          defeslotds
          `((finalize-inheritance
             (defclass ,eslotd (standard-effective-slot-definition ,slotd) 
               ,@(and defeslotds (list (cdr defeslotds)))))
            (defmethod effective-slot-definition-class ((class ,name)
                                                        &rest initargs)
              (declare (ignore initargs))
              (find-class ',eslotd)))))))

#||
(defmetaclass initialize-at-read-class ()
  ((initialize-at-read-slots :initform nil
                             :accessor class-initialize-at-read-slots))
  (:slot-definitions-mixin-slots 
   ())
  (:effective-slot-definitions-mixin-slots 
   ()))

||#