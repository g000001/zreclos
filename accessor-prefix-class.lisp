(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(defclass ~accessor-prefix-class (standard-class)
  ((accessor-prefix :initarg :accessor-prefix :initform nil)
   (reader-prefix :initarg :reader-prefix :initform nil)))


(defmethod validate-superclass ((c ~accessor-prefix-class) (s standard-class))
  T)


(defun ensure-prefixed-names (type prefix slot-code)
  (let* ((slot-code (copy-list slot-code))
         (name (getf slot-code :name))
         (readers (getf slot-code :readers))
         (writers (getf slot-code :writers)))
    (if (or readers writers)
        (let* ()
          slot-code)
        (let* ((prefix (or prefix ""))
               (reader-name (intern 
                             (concatenate 'string 
                                          (string prefix) (string name))))
               (writer-name `(setf ,reader-name)))
          (setf (getf slot-code :readers)
                `(,reader-name))
          (ecase type
            ((:reader-prefix) 'nop)
            ((:accessor-prefix)
             (setf (getf slot-code :writers)
                   `(,writer-name))))
          slot-code))))


(defmethod ensure-class-using-class ((class ~accessor-prefix-class)
                                     name &rest initargs
                                     &key (accessor-prefix nil app) (reader-prefix nil rpp))
  (if (and (null app) (null rpp))
      (let* ()
        (call-next-method))
      (let* ((initargs (copy-list initargs))
             (prefix (car accessor-prefix))
             (prefix (or prefix (car reader-prefix)))
             (type (if app :accessor-prefix :reader-prefix)))
        (apply #'call-next-method
               class
               name
               :allow-other-keys T
               :direct-slots (mapcar (lambda (slot-code)
                                       (ensure-prefixed-names type prefix slot-code))
                                     (getf initargs :direct-slots))
               initargs))))


(defclass ~accessor-prefix-object (standard-object)
  ()
  (:metaclass ~accessor-prefix-class))


;;; *EOF*
