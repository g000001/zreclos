(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(defclass ~required-slot-class (standard-class)
  ((direct-required-slots
    :initarg :required-slots
    :initform '()
    :reader ~direct-required-slots)
   (class-required-slots :accessor ~class-required-slots)))


(defmethod validate-superclass ((class ~required-slot-class)
                                (super standard-class))
  T)


(defmethod ~direct-required-slots ((class class))
  ;; return '() if not required-slot-class.
  '())


(defmethod finalize-inheritance :after ((class ~required-slot-class))
  (setf (~class-required-slots class)
        (remove-duplicates (mapcan (lambda (ds)
                                     (copy-list (~direct-required-slots ds)))
                                   (class-precedence-list class))
                           :from-end T
                           :test #'equal)))


(defmethod unbound-required-slot-using-class ((class ~required-slot-class) obj slot-name)
  (error "Required slot unbound ~A." slot-name))


(defmethod check-required-slots-using-class ((class ~required-slot-class) obj)
  (dolist (rs (~class-required-slots class))
    (unless (or (and (symbolp rs) (slot-boundp obj rs))
                (and (consp rs)
                     (eql 'or (car rs))
                     (some (lambda (s)
                             (slot-boundp obj s))
                           (cdr rs))))
      (unbound-required-slot-using-class class obj rs))))


(defmethod make-instance ((class ~required-slot-class) &rest initargs)
  (let ((instance (call-next-method)))
    (check-required-slots-using-class class instance)
    instance))


;;; *EOF*
