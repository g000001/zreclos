;;;; zreclos.lisp -*- Mode: Lisp;-*- 
(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(eval-always
(defun make-weak-vector (size &rest initargs)
  (declare (dynamic-extent initargs))
  #+lispworks
  (apply #'make-array size :weak T initargs)
  #+sbcl
  (apply #'make-array size :element-type 'sb-ext:weak-pointer initargs)))


(defmetaclass ~instance-recording-class (eclos-class)
  ((instance-record :initform (make-array 0 :adjustable T :fill-pointer 0)
                    :accessor ~class-instance-record))
  (:metaclass eclos-class))


(defmethod make-instance :around 
           ((class ~instance-recording-class) &rest initargs)
  (let* ((inst (call-next-method))
         #+sbcl (inst (sb-ext:make-weak-pointer inst)))
    (vector-push-extend inst (~class-instance-record class))
    inst))


(defun ~reset-instance-record (class)
  (setf (~class-instance-record class)
        (make-array 0 :adjustable T :fill-pointer 0)))


(declaim (inline ~scan-direct-instances))


(series::defun  ~scan-direct-instances (class)
  (declare (series:optimizable-series-function))
  (series:scan 'vector (~class-instance-record class)))


;;; *EOF*
