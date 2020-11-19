;;;; zreclos.lisp -*- Mode: Lisp;-*- 
(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(defstruct slot-init
  at
  method)


(defmetaclass ~lazy-class (~self-referent-operating-class)
  ()
  (:slot-definitions-mixin-slots
   (initialization :initform (make-slot-init)
                   :accessor slot-definition-initialization
                   :initarg :initialization))
  (:direct-slot-definitions-mixin-slots)
  (:effective-slot-definitions-mixin-slots))


(defun process-init-option (opt)
  (let ((si (make-slot-init)))
    (dolist (o opt)
      (ecase (elt o 0)
        ((:at)
         (setf (slot-init-at si)
               (elt o 1)))
        ((:before :after :follows :implies :check :unless)
         (push o (slot-init-method si)))))
    si))


(defmethod clos:process-a-slot-option 
           ((class ~lazy-class) option value already-processed-options slot)
  (if (eq option :initialization)
      (list* :initialization (process-init-option value) already-processed-options)
      (call-next-method)))


(defmethod compute-effective-slot-definition 
           ((class ~lazy-class) name direct-slot-definitions)
  (declare (ignore name))
  (let ((eslotd (call-next-method)))
    (dolist (dslotd direct-slot-definitions)
      (when (typep dslotd (find-class '~lazy-slot-definition))
        (setf (slot-definition-initialization eslotd) 
              (slot-definition-initialization dslotd))))
    eslotd))


(defmethod shared-initialize  
           ((instance ~lazy-object) slot-names &rest initargs)
  (let* ((class (class-of instance))
         (instance (call-next-method))
         (slots (class-slots class)))
    ;; lazy init
    (dolist (slotd slots)
      (setf (~instance-slot-init-option instance slotd)
             (lambda (&aux (*self-referent-object-self* instance))
              (declare (special *self-referent-object-self*))
              (block nil
                (dolist (m (slot-init-method (slot-definition-initialization slotd)))
                  (case (car m)
                    ((:after :follows)
                     (dolist (slot-name (cdr m))
                       (let ((init (~instance-slot-init-option 
                                    instance 
                                    (~find-named-slot-using-class class slot-name))))
                         (when init
                           (funcall init)))))
                    ((:check)
                     (dolist (slot-name (cdr m))
                       (when (~instance-slot-init-option-boundp 
                              instance
                              (~find-named-slot-using-class class slot-name))
                         (return))))
                    ((:unless)
                     (dolist (slot-name (cdr m))
                       (unless (~instance-slot-init-option-boundp
                                instance
                                (~find-named-slot-using-class class slot-name))
                         (return))))))
                (unless (initialize-slot-from-initarg class instance slotd initargs)
                  (when (or (eq T slot-names)
                            (member (slot-definition-name slotd) slot-names))
                    (initialize-slot-from-initfunction class instance slotd)))
                (dolist (m (slot-init-method (slot-definition-initialization slotd)))
                  (case (car m)
                    ((:before :implies)
                     (dolist (slot-name (cdr m))
                       (let ((init (~instance-slot-init-option 
                                    instance
                                    (~find-named-slot-using-class class slot-name))))
                         (when init
                           (funcall init)))))))))))
    ;; eager init
    (dolist (slotd slots)
      (when (null (slot-init-at (slot-definition-initialization slotd)))
        (funcall (~instance-slot-init-option instance slotd)))))
  instance)


(defmethod slot-value-using-class ((class ~lazy-class) instance (slotd slot-definition))
  (case (slot-init-at (slot-definition-initialization slotd))
    ((:read :access)
     (when (~instance-slot-init-option instance slotd) 
       (funcall (~instance-slot-init-option instance slotd))
       (setf (~instance-slot-init-option instance slotd)
             nil)))
    (otherwise nil))
  (~instance-slot-value instance slotd))


(defmethod (setf slot-value-using-class)
           (val (class ~lazy-class) instance (slotd slot-definition))
  (case (slot-init-at (slot-definition-initialization slotd))
    ((:read :access)
     (when (~instance-slot-init-option instance slotd) 
       (funcall (~instance-slot-init-option instance slotd))
       (setf (~instance-slot-init-option instance slotd)
             nil)))
    (otherwise nil))
  (~instance-slot-value instance slotd))


;;; *EOF*
