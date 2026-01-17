(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(defmetaclass ~attributed-class (~self-referent-operating-class)
  ((default-attributes :initform '()
                       :initarg :default-attributes
                       :accessor ~class-default-attributes))
  (:metaclass eclos-class))


(defgeneric ~slot-attribute-using-class (class instance slotd))


(defmethod ~slot-attribute-using-class
           ((class ~attributed-class) instance (slotd slot-definition))
  (~instance-slot-attribute instance slotd))


(defun ~slot-attribute (instance slot-name)
  (let ((class (class-of instance)))
    (~slot-attribute-using-class class
                                 instance
                                 (~find-named-slot-using-class class slot-name))))


(defun (setf ~slot-attribute) (val instance slot-name)
  (let ((class (class-of instance)))
    (setf (~slot-attribute-using-class class
                                       instance
                                       (~find-named-slot-using-class class slot-name))
          val)))


(defgeneric (setf ~slot-attribute-using-class) (val class instance slotd))


(defmethod (setf ~slot-attribute-using-class)
           (val (class ~attributed-class) instance (slotd slot-definition))
  (setf (~instance-slot-attribute instance slotd)
        val))


(defclass ~attributed-slot-definition (standard-slot-definition)
  ((attributes :initform nil
               :initarg :attributes 
               :accessor ~attributed-slot-definition-attributes)))


(defclass ~direct-slot/attribute-definition
          (standard-direct-slot-definition ~attributed-slot-definition)
  ())


(defmethod direct-slot-definition-class ((class ~attributed-class) &rest initargs)
  (find-class '~direct-slot/attribute-definition))


(defmethod clos:process-a-slot-option ((class ~attributed-class) option value
                                       already-processed-options slot)
  (if (eq option :attributes)
      (list* :attributes
             `(let ((c (defclass ,(gensym (format nil
                                                  "ATTRIBUTED-CLASS.~A-" 
                                                  (string (car slot))))
                         (~attributed-object)
                         ,value
                         (:metaclass ~attributed-class))))
                (finalize-inheritance c)
                c)
             already-processed-options)
      (call-next-method)))


(defmethod clos:process-a-class-option ((class ~attributed-class)
                                        (name (eql :default-attributes))
                                        value)
  (unless (and value (null (cdr value)))
    (error "attributed-class :default-attributes must have a single value."))
  (list name
        `(let ((c (defclass ,(gensym "DEFAULT-ATTRIBUTES-") (~attributed-object)
                    ,(car value)
                    (:metaclass ~attributed-class))))
           (finalize-inheritance c)
           c)))


(defclass ~effective-slot/attribute-definition
          (standard-effective-slot-definition ~attributed-slot-definition)
  ())


(defmethod effective-slot-definition-class 
           ((class ~attributed-class) &rest initargs)
  (find-class '~effective-slot/attribute-definition))


(defmethod compute-effective-slot-definition ((class ~attributed-class)
                                              name
                                              direct-slot-definitions)
  (let ((effective-slotd (call-next-method)))
    (dolist (slotd direct-slot-definitions)
      (when (typep slotd '~attributed-slot-definition)
        (setf (~attributed-slot-definition-attributes effective-slotd) 
              (~attributed-slot-definition-attributes slotd))
        (return)))
    effective-slotd))


(defmethod shared-initialize :after ((instance ~attributed-object) slot-names &rest initargs)
  (let* ((class (class-of instance))
         (slots (class-slots class))
         (default-attributes (~class-default-attributes class)))
    (dolist (s slots)
      (let ((attr (~attributed-slot-definition-attributes s)))
        (if attr
            (setf (~slot-attribute-using-class class instance s)
                  (make-instance (~attributed-slot-definition-attributes s)))
            (and default-attributes
                 (setf (~slot-attribute-using-class class instance s)
                       (make-instance default-attributes))))))))


(defun ~attribute-value (instance &rest names)
  (let ((ans instance))
    (mapl (lambda (n)
            (if (cdr n)
                (setq ans (~slot-attribute ans (car n)))
                (setq ans (slot-value ans (car n)))))
          names)
    ans))


;;; *EOF*


