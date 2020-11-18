;;;; zreclos.lisp -*- Mode: Lisp;-*- 
(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(defmetaclass ~operating-class (standard-class)
  ((container-types :initform '(~operating-object)
                    :accessor class-container-types
                    :initarg :container-types))
  (:metaclass standard-class))


(defun alloc-fix-instance (wrapper instance-slots)
  #+allegro
  (excl::.primcall 'sys::new-standard-instance
                   wrapper
                   instance-slots)
  #+lispworks
  (sys:alloc-fix-instance wrapper instance-slots)
  #+sbcl
  (let* ((instance (sb-pcl::%make-instance (1+ sb-vm:instance-data-start))))
    (setf (sb-kernel::%instance-layout instance) wrapper)
    (setf (sb-pcl::std-instance-slots instance) instance-slots)
    instance)
  #+ccl
  (let ((instance (ccl::gvector :instance 0 wrapper nil)))
    (setf (ccl::instance.hash instance) (ccl::strip-tag-to-fixnum instance)
	  (ccl::instance.slots instance) instance-slots)
    instance))


(defun class-wrapper (class)
  #+allegro (excl::class-wrapper class)
  #+lispworks (clos::class-wrapper class)
  #+sbcl (sb-pcl::class-wrapper class)
  #+ccl (ccl::instance-class-wrapper class))


#++(class-wrapper (find-class 'standard-object))

(defun instance-wrapper (ins)
  #+allegro (excl::std-instance-wrapper ins)
  #+lispworks (clos::standard-instance-wrapper ins)
  #+sbcl (sb-kernel::%instance-layout ins)
  #+ccl (ccl::instance.class-wrapper ins))


(defun instance-slots (ins)
  #+allegro (excl::std-instance-slots ins)
  #+lispworks (clos::standard-instance-static-slots ins)
  #+sbcl (sb-pcl::std-instance-slots ins)
  #+ccl (ccl::instance.slots ins))


#|(instance-wrapper
 (alloc-fix-instance (class-wrapper (find-class 'standard-object))
                     #(a b c)))

(c2mop:standard-instance-access
 (alloc-fix-instance (class-wrapper (find-class 'standard-object))
                     #(a b c))
 0)

(instance-slots
 (alloc-fix-instance (class-wrapper (find-class 'standard-object))
                     '(a b c)))|#


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
           ((instance ~operating-object) slot-names &rest initargs)
  (let ((class (class-of instance)))
    (dolist (slotd (class-slots class))
      (unless (initialize-slot-from-initarg class instance slotd initargs)
        (when (or (eq T slot-names)
                  (member (slot-definition-name slotd) slot-names))
          (initialize-slot-from-initfunction class instance slotd)))))
  instance)


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


(defun find-slot-definition (slot-name slotds &optional (no-error-p nil))
  (cond ((loop :for slotd :in slotds
               :thereis (and (eq slot-name (slot-definition-name slotd))
                             slotd)))
        (no-error-p nil)
        (t (error "~A is not the name of a slotd." slot-name))))


;;; *EOF*
