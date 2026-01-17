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
  (:effective-slot-definitions-mixin-slots)
  (:metaclass eclos-class))


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
         (slots (class-slots class)))
    ;; lazy init
    (dolist (slotd slots)
      (let ((slotd slotd))
        (setf (~instance-slot-init-option instance slotd)
              (lambda (&aux (*self-referent-object-self* instance))
                (declare (special *self-referent-object-self*))
                (labels ((base-init ()
                           (unless (initialize-slot-from-initarg class instance slotd
                                                                 initargs)
                             (when (or (eq T slot-names)
                                       (member (slot-definition-name slotd) slot-names))
                               (initialize-slot-from-initfunction class instance slotd)))
                           (setf (init-opt-initfun 
                                  (slot-definition-name slotd))
                                 nil))
                         (init-opt-initfun (slot-name)
                           (~instance-slot-init-option instance 
                                                       (~find-named-slot-using-class 
                                                        class
                                                        slot-name)))
                         ((setf init-opt-initfun) (val slot-name)
                           (setf (~instance-slot-init-option instance 
                                                             (~find-named-slot-using-class 
                                                              class
                                                              slot-name))
                                 val)))
                  (or (dolist (m (slot-init-method (slot-definition-initialization slotd))
                                 NIL)
                        (ecase (car m)
                          ((:after)
                           ;; Each one of the named slots 
                           ;;  which is still uninitialized 
                           ;;  at the time initialization of the specifying slot is due, 
                           ;;  gets initialized before it, too.
                           (dolist (slot-name (cdr m))
                             (let ((init (init-opt-initfun slot-name)))
                               (when init
                                 (funcall init)
                                 (setf (init-opt-initfun slot-name) nil))))
                           (base-init))
                          ((:follows)
                           ;; The specifying slot gets initialized right after
                           ;;  the rest of the named slots to get initialized.
                           (dolist (slot-name (cdr m))
                             (let ((init (init-opt-initfun slot-name)))
                               (when init
                                 (setf (init-opt-initfun slot-name)
                                       (lambda ()
                                         (funcall init)
                                         (base-init)))))))
                          ((:check)
                           ;; The slot is initialized only if
                           ;;   the named slots are already initialized.
                           (and (dolist (slot-name (cdr m) T)
                                  (when (init-opt-initfun slot-name)
                                    (return)))
                                (base-init)))
                          ((:unless)
                           ;; The slot is initialized only if
                           ;;  none of the named slots are already initialized.
                           (and (dolist (slot-name (cdr m) T)
                                  (unless (init-opt-initfun slot-name)
                                    (return)))
                                (base-init)))
                          ((:before)
                           ;; This is equivalent to listing 
                           ;;  the specifying slot in an :after initialization option of 
                           ;;  the named slots.
                           (dolist (slot-name (cdr m))
                             (let ((init (init-opt-initfun slot-name)))
                               (when init
                                 (setf (init-opt-initfun slot-name)
                                       (lambda ()
                                         (base-init)
                                         (funcall init)))))))
                          ((:implies)
                           ;; All of the named slots that remain uninitialized 
                           ;;  at the time the specifying slot gets initialized 
                           ;;  are initialized after it.
                           (base-init)
                           (dolist (slot-name (cdr m))
                             (let ((init (init-opt-initfun slot-name)))
                               (when init
                                 (funcall init)))))))
                      (base-init)))))))
    ;; eager init
    (dolist (slotd slots)
      (when (null (slot-init-at (slot-definition-initialization slotd)))
        (funcall (~instance-slot-init-option instance slotd)))))
  instance)


(defmethod slot-value-using-class ((class ~lazy-class) instance (slotd slot-definition))
  (case (slot-init-at (slot-definition-initialization slotd))
    ((:read :access)
     (when (~instance-slot-init-option instance slotd) 
       (funcall (~instance-slot-init-option instance slotd))))
    (otherwise nil))
  (~instance-slot-value instance slotd))


(defmethod (setf slot-value-using-class)
           (val (class ~lazy-class) instance (slotd slot-definition))
  (case (slot-init-at (slot-definition-initialization slotd))
    ((:read :access)
     (when (~instance-slot-init-option instance slotd) 
       (funcall (~instance-slot-init-option instance slotd))))
    (otherwise nil))
  (~instance-slot-value instance slotd))


;;; *EOF*
