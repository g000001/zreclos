;;;; zreclos.lisp -*- Mode: Lisp;-*- 
(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(defmetaclass ~operating-class (standard-class)
  ((container-types :initform '(~operating-object)
                    :accessor class-container-types
                    :initarg :container-types))
  (:metaclass standard-class))


(defclass ~operating-slot-definition (standard-slot-definition)
  ())

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


(defconstant ~operating-object-tracks 4)


(defconstant ~values-track 0)


(defconstant ~attributes-track 1)


(defconstant ~init-options-track 2)


(defconstant ~constraints-track 3)


(defun allocate-operating-object-vector (nfields)
  (make-array (list 4 nfields) 
              :initial-element
              #+lispworks clos::*slot-unbound*
              #+sbcl (sb-pcl::make-unbound-marker)))


(defmethod allocate-instance ((class ~operating-class) &rest initargs)
  (alloc-fix-instance (class-wrapper class)
                      (let* ((nfields (length (class-slots class)))
                             (a (make-array `(,~operating-object-tracks ,nfields) 
                                            :initial-element
                                            #+lispworks clos::*slot-unbound*
                                            #+sbcl (sb-pcl::make-unbound-marker))))
                        a)))


(defmacro define-track-accessor (track)
  `(progn
     (defun ,(intern (concatenate 'string (string 'instance-slot-) (string track))
                     'zreclos)
            (instance slotd)
       (aref (instance-slots instance) 
             ,(intern (concatenate 'string (string track) (string 's-track))
                      'zreclos)
             (slot-definition-location slotd)))
     (defun (setf ,(intern (concatenate 'string (string 'instance-slot-) (string track))
                           'zreclos))
            (value instance slotd)
       (setf (aref (instance-slots instance) 
                   ,(intern (concatenate 'string (string track) (string 's-track))
                            'zreclos)
                   (slot-definition-location slotd))
             value))))


(define-track-accessor value)


(define-track-accessor attribute)


(define-track-accessor init-option)


(defun ~instance-slot-init-option-boundp (instance slotd)
  (eq #+lispworks clos::*slot-unbound*
      (~instance-slot-init-option 
       instance
       slotd)))


(define-track-accessor constraint)


(defmethod slot-value-using-class
           ((class ~operating-class) instance (slotd slot-definition))
  (~instance-slot-value instance slotd))


(defmethod (setf slot-value-using-class)
           (val (class ~operating-class) instance (slotd slot-definition))
  (setf (~instance-slot-value instance slotd)
        val))


(defgeneric initialize-slot-from-initarg (class instance slotd initargs))


(defmethod initialize-slot-from-initarg (class instance slotd initargs)
  (declare (ignore class))
  (let ((slot-initargs (slot-definition-initargs slotd)))
    (loop :for (initarg value) :on initargs :by #'cddr
          :do (when (member initarg slot-initargs)
                (setf (~instance-slot-value instance slotd)
                      value)
                (return T)))))


(defgeneric initialize-slot-from-initfunction (class instance slotd))


(defmethod initialize-slot-from-initfunction (class instance slotd)
  (declare (ignore class))
  (let ((initfun (slot-definition-initfunction slotd)))
    (unless (not initfun)
      (setf (~instance-slot-value instance slotd)
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
    (dolist (slotd (class-slots class))
      (setf (~instance-slot-value new slotd)
            (funcall fn (~instance-slot-value obj slotd))))
    new))


(defgeneric ~walkslots (class fn obj))


(defmethod ~walkslots ((class ~operating-class)
                       (fn function)
                       (obj ~operating-object))
  (let* ((class (class-of obj)))
    (dolist (slotd (class-slots class))
      (funcall fn (~instance-slot-value obj slotd)))
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


(defun ~find-named-slot-using-class (class slot-name &optional (no-error-p nil))
  #+lispworks
  (let ((wrapper (class-wrapper class))
        (pos nil))
    (cond ((setq pos (position slot-name (elt wrapper 1)))
           (elt (elt wrapper 4) pos))
          (no-error-p nil)
          (t (error "~A is not the name of a slotd." slot-name))))
  #-(or lispworks)
  (cond ((loop :for slotd :in (class-slots class)
               :thereis (and (eq slot-name (slot-definition-name slotd))
                             slotd)))
        (no-error-p nil)
        (t (error "~A is not the name of a slotd." slot-name))))


;;; *EOF*
