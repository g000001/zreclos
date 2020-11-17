(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(defclass ~slotted-class (standard-class)
  ())


(defmethod validate-superclass ((class ~slotted-class) (super standard-class))
  T)


(defclass ~slotted-object (standard-object)
  ()
  (:metaclass ~slotted-class))


(defun initialize-slot-from-initarg (class instance slotd initargs)
  (let ((slot-initargs (slot-definition-initargs slotd)))
    (loop :for (initarg value) :on initargs :by #'cddr
          :do (when (member initarg slot-initargs)
                (setf (slot-value-using-class class instance slotd)
                      value)
                (return T)))))


(defun initialize-slot-from-initfunction (class instance slotd)
  (let ((initfun (slot-definition-initfunction slotd)))
    (unless (not initfun)
      (setf (slot-value-using-class class instance slotd)
            (funcall initfun)))))


(defmethod shared-initialize 
           ((instance ~slotted-object) slot-names &rest initargs)
  (let ((class (class-of instance)))
    (dolist (slotd (class-slots class))
      (unless (initialize-slot-from-initarg class instance slotd initargs)
        (when (or (eq T slot-names)
                  (member (slot-definition-name slotd) slot-names))
          (initialize-slot-from-initfunction class instance slotd)))))
  instance)


;;; *EOF*
