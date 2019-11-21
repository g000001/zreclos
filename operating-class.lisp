;;;; zreclos.lisp -*- Mode: Lisp;-*- 
(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(defclass ~operating-class (standard-class)
  ((walkers :initform `((~operating-object mapslots . walkslots))
            :accessor class-walkers))
  (:metaclass standard-class))


(defmethod validate-superclass
           ((c ~operating-class) (s standard-class))
  T)


(defclass ~operating-object (standard-object)
  ()
  (:metaclass ~operating-class))

(defgeneric mapslots (fn obj))

(defmethod mapslots ((fn function) (obj ~operating-object))
  (let* ((class (class-of obj))
         (new (allocate-instance class)))
    (dotimes (i (length (class-slots class)))
      (setf (standard-instance-access new i)
            (funcall fn (standard-instance-access obj i))))
    new))


(defgeneric walkslots (fn obj))

(defmethod walkslots ((fn function) (obj ~operating-object))
  (let* ((class (class-of obj)))
    (dotimes (i (length (class-slots class)))
      (funcall fn (standard-instance-access obj i)))
    obj))

(defgeneric mapslots* (fn obj))

(defmethod mapslots* ((fn function) (obj ~operating-object))
  (mapslots (lambda (o)
              (let ((walker (find-if (lambda (x)
                                       (subtypep (type-of o) (car x)))
                                     (class-walkers (class-of obj)))))
                (if walker
                    (funcall (car (cdr walker)) fn o)
                    (funcall fn o))))
            obj))

(defgeneric walkslots* (fn obj))



(defmethod walkslots* ((fn function) (obj ~operating-object))
  (walkslots (lambda (o)
               (let ((walker (find-if (lambda (x)
                                        (subtypep (type-of o) (car x)))
                                      (class-walkers (class-of obj)))))
                 (if walker
                     (funcall (cdr (cdr walker)) fn o)
                     (funcall fn o))))
             obj))

#||||||||||||||||;

(~defclass qqq (~operating-object)
  ((a :initform 0)
   (b :initform 0)
   (c :initform 0)))

(class-walkers (find-class 'qqq))
 
(let ((obj (make-instance 'qqq)))
  (with-slots (a b c) obj
    (setq a (make-instance 'qqq))
    (setq b (make-instance 'qqq))
    (setq c 42)
    (let ((ans nil))
      (walkslots* (lambda (x) (push x ans))
                  (mapslots* #'values obj))
      ans)))

(42 0 0 0 0 0 0) 
 

(let ((obj (make-instance 'qqq)))
  (with-slots (a b c) obj
    (setq a (make-instance 'qqq))
    (setq b (make-instance 'qqq))
    (setq c 42)
    (let ((ans nil))
      (walkslots* (lambda (x) (push x ans))
                  (mapslots* #'list obj))
      ans)))

((42) (0) (0) (0) (0) (0) (0)) 

 

(let ((obj (make-instance 'qqq)))
  (setf (class-walkers (find-class 'qqq))
        '((zreclos::operating-object mapslots . walkslots)
          (list mapcar . mapc)))
  (with-slots (a b c) obj
    (setq a (make-instance 'qqq))
    (setq b (make-instance 'qqq))
    (setq c 42)
    '(let ((ans nil))
      (walkslots* (lambda (x) (push x ans))
                  (mapslots* #'list obj))
      ans)
    (let ((ans nil))
      (walkslots* (lambda (x) (push x ans))
                  (mapslots* #'list obj))
      ans)))

(42 (0) (0) (0) (0) (0) (0)) 


;||||||||||||||||#



;;; *EOF*
