(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(defvar *self-referent-object-self*)


(~defmetaclass ~self-referent-class (eclos-class)
  ())


(defclass ~self-referent-slot-definition (eclos-slot-definition)
  ())


(defun make-creator-function-form (slot-form)
  (let ((name (if (consp slot-form)
                  (car slot-form)
                  slot-form)))
    `(,name (~self) (slot-value ~self ',name))))


(defmethod clos::expand-defclass 
           ((prototype ~self-referent-class) metaclass name superclasses slots class-options)
  (destructuring-bind (eval-when opts &body body)
                      (call-next-method)
    `(,eval-when ,opts
       (flet (,@(mapcar #'make-creator-function-form slots))
         ,@body))))


(defmethod shared-initialize :around ((instance ~self-referent-object) slot-names &rest initargs)
  (let ((*self-referent-object-self* instance))
    (declare (special *self-referent-object-self*))
    (call-next-method)))


;; from alexandria
(defun flatten (tree)
  "Traverses the tree in order, collecting non-null leaves into a list."
  (let (list)
    (labels ((traverse (subtree)
               (when subtree
                 (if (consp subtree)
                     (progn
                       (traverse (car subtree))
                       (traverse (cdr subtree)))
                     (push subtree list)))))
      (traverse tree))
    (nreverse list)))


(defun non-trivial-initform-initfunction-p (initform)
  #+(or lispworks7.1 lispworks8.0 lispworks8.1)
  (loop :for (name ntifif) :on (flatten initform)
        :thereis (and (eq 'hcl:lambda-name name)
                      (eq 'clos::non-trivial-initform-initfunction ntifif)))
  #+lispworks7.0
  (let ((x initform))
    (and (consp x)
         (eq 'function (car x))
         (eq 'lambda (caadr x)))))


(defgeneric make-sr-class-initfunction-form (class ifform))


(defmethod make-sr-class-initfunction-form ((class ~self-referent-class)
                                            ifform)
  (if (non-trivial-initform-initfunction-p ifform)
      (destructuring-bind (function (lambda arg &body body))
                          ifform
        (declare (ignore arg))
        `(,function (,lambda (&aux (~self *self-referent-object-self*)) 
                             (declare (special *self-referent-object-self*))
                             (declare (ignorable ~self))
                             ,@body)))
      ifform))


(defmethod clos::canonicalize-defclass-slot
           ((prototype ~self-referent-class) slot)
  (let* ((plist (copy-list (cdr (call-next-method))))
         (ifform (getf plist :initfunction)))
    (if (getf plist :initform)
        (progn
          (remf plist :initfunction)
          `(list ,@plist 
                 :initfunction ,(make-sr-class-initfunction-form prototype ifform)))
        (progn
          `(list ,@plist)))))


;;; *EOF*
