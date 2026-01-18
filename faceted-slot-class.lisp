;;;; zreclos.lisp -*- Mode: Lisp;-*- 
(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(defgeneric ~index-in-instance (class description))


(defmethod ~index-in-instance ((class cl:standard-class) description)
  (typecase description
    (symbol
     (position description (class-slots class)
               :key #'slot-definition-name))
    (T (error "Don't understand the description ~S." description))))


(defgeneric ~compute-instance-size (class))


(defmethod ~compute-instance-size ((class cl:standard-class))
  (length (class-slots class)))


(~defmetaclass ~faceted-slot-class () ())


(defmethod ~compute-instance-size ((class ~faceted-slot-class))
  (* 2 (call-next-method)))


(defmethod allocate-instance ((class ~faceted-slot-class) &rest initargs)
  (let ((class (clos::ensure-class-finalized class)))
    (sys:alloc-fix-instance (clos::class-wrapper class)
                            (sys:alloc-g-vector$fixnum (~compute-instance-size class)
                                                       clos::*slot-unbound*))))


(defmethod ~index-in-instance ((class ~faceted-slot-class) description)
  (cond ((symbolp description)
         (let ((index (call-next-method)))
           (and index (* 2 index))))
        ((and (consp description)
              (eq (car description) 'facet))
         (1+ (~index-in-instance class (cadr description))))
        (T
         (error "Don't understand the description ~S." description))))


(defun ~faceted-slot-instance-access (instance description trap not-bound-function missing-function)
  (declare (ignore trap))
  (let* ((class (class-of instance))
         (index (~index-in-instance class description)))
    (cond ((null index)
           (funcall missing-function instance description))
      ((not (numberp index))
       (slot-value index 'value))
          ((null (~standard-instance-boundp instance index))
           (funcall not-bound-function instance description))
          (T
           (standard-instance-access instance index)))))


(defun (setf ~faceted-slot-instance-access) (val instance description trap not-bound-function missing-function)
  (declare (ignore trap not-bound-function))
  (let* ((class (class-of instance))
         (index (~index-in-instance class description)))
    (cond ((null index)
           (funcall missing-function instance description))
      ((not (numberp index))
       (slot-value index 'value))
          (T
           (setf (standard-instance-access instance index) val)))))


(defun ~standard-instance-boundp (instance index)
  (not (eq clos::*slot-unbound* (standard-instance-access instance index))))


(defun ~slot-facet (instance slot-name)
  (~faceted-slot-instance-access instance
                                 (list 'facet slot-name)
                                 nil
                                 #'~facet-unbound
                                 #'~facet-missing))


(defun (setf ~slot-facet) (new-value instance slot-name)
  (setf (~faceted-slot-instance-access instance
                                       (list 'facet slot-name)
                                       nil
                                       #'~facet-unbound
                                       #'~facet-missing)
        new-value))


(defun ~facet-unbound (instance facet)
  (error "The facet ~S is unbound in the object ~S" (cadr facet) instance))


(defun ~facet-missing (instance facet)
  (error "The facet ~S is missing from the object ~S" (cadr facet) instance))


(defmethod compute-slots :around ((class ~faceted-slot-class))
  (let ((slotds (call-next-method)))
    (dolist (s slotds)
      ;; Base case
      (setf (slot-definition-location s)
            (* 2 (position s slotds))))
    slotds))


(defmethod slot-value-using-class ((class ~faceted-slot-class) instance slot-name)
  (let ((index (~index-in-instance class slot-name)))
    (cond ((null index)
           (slot-missing class instance slot-name 'slot-makunbound))
      ((not (numberp index))
       (slot-value index 'value))
          ((null (~standard-instance-boundp instance index))
           (slot-unbound class instance slot-name))
          (T
           (standard-instance-access instance index)))))


(defmethod describe-object ((obj ~faceted-slot-object) stream)
  (format stream "~&~S ~A" obj (sys::is-a (string (type-of obj))))
  (let ((class (class-of obj)))
    (dolist (s (class-slots class))
      (let ((name (slot-definition-name s)))
        (let ((slotval (elt (clos::%svref obj 1)
                            (slot-definition-location s)))
              (facetval (elt (clos::%svref obj 1)
                             (1+ (slot-definition-location s)))))
          (format stream
                  sys::*default-describe-object-label*
                  name
                  6)
          (if (eq clos::*slot-unbound* slotval)
              (write-string "#<unbound slot>" stream)
              (format stream "~S" slotval))
          (format stream
                  "~% ~S ~VT "
                  name
                  6)
          (if (eq clos::*slot-unbound* facetval)
              (write-string "#<unbound facet>" stream)
              (format stream "~S" facetval)))))
    (terpri stream)))


;;; *EOF*
