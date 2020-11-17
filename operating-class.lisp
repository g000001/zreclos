;;;; zreclos.lisp -*- Mode: Lisp;-*- 
(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(defclass ~operating-class (standard-class)
  ((container-types :initform '(~operating-object)
                    :accessor class-container-types
                    :initarg :container-types))
  (:metaclass standard-class))


(defmethod validate-superclass
           ((c ~operating-class) (s standard-class))
  T)


(defclass ~operating-object (standard-object)
  ()
  (:metaclass ~operating-class))


(defgeneric ~mapslots (class fn obj))


(defmethod ~mapslots ((class ~operating-class)
                     (fn function)
                     (obj ~operating-object))
  (let* ((new (allocate-instance class)))
    (dotimes (i (length (class-slots class)))
      (setf (standard-instance-access new i)
            (funcall fn (standard-instance-access obj i))))
    new))


(defgeneric ~walkslots (class fn obj))


(defmethod ~walkslots ((class ~operating-class)
                      (fn function)
                      (obj ~operating-object))
  (let* ((class (class-of obj)))
    (dotimes (i (length (class-slots class)))
      (funcall fn (standard-instance-access obj i)))
    obj))


(defgeneric ~mapslots* (class fn obj))


(defmethod ~mapslots* ((class ~operating-class)
                      (fn function)
                      obj)
  (funcall fn obj))


(defmethod ~mapslots* ((class ~operating-class)
                      (fn function)
                      (obj ~operating-object))
  (~mapslots class
            (lambda (o)
              (~mapslots* class fn o))
            obj))


(defgeneric ~walkslots* (class fn obj))


(defmethod ~walkslots* ((class ~operating-class)
                       (fn function)
                       (obj ~operating-object))
  (~walkslots class
             (lambda (o)
               (~walkslots* class fn o))
             obj))


(defmethod ~walkslots* ((class ~operating-class)
                      (fn function)
                      obj)
  (funcall fn obj))


;;; *EOF*
