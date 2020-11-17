(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(~defclass ~attributed-class (~slotted-class ~self-referent-class)
  ((default-attributes :initform '()
                       :initarg :default-attributes
                       :accessor ~class-default-attributes)))


(defclass ~attributed-object (~slotted-object)
  ()
  (:metaclass ~attributed-class))


(defmethod allocate-instance ((class ~attributed-class) &rest initargs)
  (alloc-fix-instance (class-wrapper class)
                      (let* ((nfields (length (class-slots class)))
                             (a (make-array (list 2 nfields) 
                                            :initial-element
                                            #+lispworks clos::*slot-unbound*
                                            #+sbcl (sb-pcl::make-unbound-marker))))
                        a)))


(defun ~attributed-instance-slot-access (instance index)
  (aref (standard-instance-slots instance) 
        0
        index))


(defun ~attributed-instance-attribute-access (instance index)
  (aref (standard-instance-slots instance) 
        1
        index))


(defmethod slot-value-using-class
           ((class ~attributed-class) instance (slotd slot-definition))
  (~attributed-instance-slot-access instance
                                    (slot-definition-location slotd)))


(defgeneric ~slot-attribute-using-class (class instance slotd))


(defmethod ~slot-attribute-using-class
           ((class ~attributed-class) instance (slotd slot-definition))
  (~attributed-instance-attribute-access instance
                                         (slot-definition-location slotd)))


(defun ~slot-attribute (instance slot-name)
  (let ((class (class-of instance)))
    (~slot-attribute-using-class class
                                 instance
                                 (find slot-name
                                       (class-slots class)
                                       :key #'slot-definition-name))))


(defun (setf ~attributed-instance-slot-access) (val instance index)
  (setf (aref (standard-instance-slots instance) 
              0
              index)
        val))


(defun (setf ~attributed-instance-attribute-access) (val instance index)
  (setf (aref (standard-instance-slots instance) 
              1
              index)
        val))


(defmethod (setf slot-value-using-class)
           (val (class ~attributed-class) instance (slotd slot-definition))
  (setf (~attributed-instance-slot-access instance
                                          (slot-definition-location slotd))
        val))


(defgeneric (setf ~slot-attribute-using-class) (val class instance slotd))


(defmethod (setf ~slot-attribute-using-class)
           (val (class ~attributed-class) instance (slotd slot-definition))
  (setf (~attributed-instance-attribute-access instance
                                               (slot-definition-location slotd))
        val))


(defun (setf ~slot-attribute) (val instance slot-name)
  (let ((class (class-of instance)))
    (setf (~slot-attribute-using-class class
                                       instance
                                       (find slot-name
                                             (class-slots class)
                                             :key #'slot-definition-name))
          val)))


(defclass ~slot/attribute-definition (standard-slot-definition)
  ((attributes :initform nil
               :initarg :attributes 
               :accessor ~slot/attribute-definition-attributes)))


(defclass ~direct-slot/attribute-definition
          (standard-direct-slot-definition ~slot/attribute-definition)
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
        `(let ((c (defclass ,(gensym "DEFAULT-ATTRIBUTES-")
                      (~attributed-object)
                    ,(car value)
                    (:metaclass ~attributed-class))))
           (finalize-inheritance c)
           c)))


(defclass ~effective-slot/attribute-definition
          (standard-effective-slot-definition ~slot/attribute-definition)
  ())


(defmethod effective-slot-definition-class 
           ((class ~attributed-class) &rest initargs)
  (find-class '~effective-slot/attribute-definition))


(defmethod compute-effective-slot-definition ((class ~attributed-class)
                                              name
                                              direct-slot-definitions)
  (let ((effective-slotd (call-next-method)))
    (dolist (slotd direct-slot-definitions)
      (when (typep slotd 'slot/attribute-definition)
        (setf (slot/attribute-definition-attributes effective-slotd) 
              (slot/attribute-definition-attributes slotd))
        (return)))
    effective-slotd))


(defmethod shared-initialize ((instance ~attributed-object) slot-names &rest initargs)
  (let* ((class (class-of instance))
         (slots (class-slots class))
         (instance (call-next-method))
         (default-attributes (~class-default-attributes class)))
    (dolist (s slots)
      (let ((attr (~slot/attribute-definition-attributes s)))
        (if attr
            (setf (~slot-attribute-using-class class instance s)
                  (make-instance (~slot/attribute-definition-attributes s)))
            (and default-attributes
                 (setf (~slot-attribute-using-class class instance s)
                       (make-instance default-attributes))))))
    instance))


(defun ~attribute-value (instance &rest names)
  (let ((ans instance))
    (mapl (lambda (n)
            (if (cdr n)
                (setq ans (~slot-attribute ans (car n)))
                (setq ans (slot-value ans (car n)))))
          names)
    ans))


;;; *EOF*
