;;;; test.lisp -*- Mode: Lisp;-*- 

(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(def-suite zreclos)


(in-suite zreclos)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:defclass a-class (standard-class)
  ()
  (:metaclass standard-class))


(cl:defclass b-class (standard-class)
  ()
  (:metaclass standard-class))


(defmethod validate-superclass ((c a-class) (s standard-class)) T)


(defmethod validate-superclass ((c b-class) (s standard-class)) T)


(cl:defclass c-class (a-class b-class)
  ()
  (:metaclass standard-class))


(defmethod validate-superclass ((c c-class) (s standard-class)) T)


(defconstant <a>
  (~defclass a ()
    ()
    (:metaclass a-class)))


(defconstant <b>
  (~defclass b ()
    ()
    (:metaclass b-class)))


(test |compute-metaclass test|
  (defconstant <c>
    (~defclass c (a b)
      ()
      (:metaclass c-class))) ;TODO
  (defparameter *z* (gensym "z"))
  (defparameter *y* (gensym "y"))
  (ensure-class *z* :direct-superclasses (list *y*))
  (is (eq (compute-metaclass '())
          (find-class 'standard-class)))
  (is (eq (compute-metaclass '() :default-metaclass-name 'standard-class)
          (find-class 'standard-class)))
  (is (eq (compute-metaclass (list <a> <b>))
          (find-class 'c-class)))
  (is (eq (class-of <c>)
          (find-class 'c-class)))
  (is (eq (class-of (find-class *y*))
          (find-class 'forward-referenced-class)))
  (is (eq (class-of (ensure-class *y*))
          (find-class 'cl:standard-class))))


(test |self-referent-class test|
  (defun make-point (x y)
    (list x y))
  (~defclass (horizontal-line :stklos) (~self-referent-object)
    ((x1 :accessor x1 :initarg :x1 :type real)
     (x2 :accessor x2 :initarg :x2 :type real)
     (y :accessor y :initarg :y :type real)
     (point1 :initform (make-point (x1 ~self)
                                   (y ~self)))
     (point2 :initform (make-point (x2 ~self)
                                   (y ~self)))))
  (let ((obj (make-instance 'horizontal-line :x1 1 :x2 2 :y 3)))
    (is (equal 1 (slot-value obj 'x1)))
    (is (equal 2 (slot-value obj 'x2)))
    (is (equal '(1 3) (slot-value obj 'point1)))
    (is (equal '(2 3) (slot-value obj 'point2)))))


(test |instance-recording-class test|
  (~defclass irobj (~instance-recording-object) 
    ()
    (:metaclass ~instance-recording-class))
  (~reset-instance-record (find-class 'irobj))
  (let ((size 100))
    (dotimes (i size)
      (make-instance 'irobj))
    (is (= size
           (series:collect-length
            (~scan-direct-instances
             (find-class 'irobj)))))))


(test |operating-class test|
  (defconstant <qqq>
    (~defclass qqq (~operating-object)
      ((a :initform 0)
       (b :initform 0)
       (c :initform 0))
      ;;(:container-types qqq list)
      (:metaclass ~operating-class)))
  (is (equal
       (let ((obj (make-instance 'qqq)))
         (with-slots (a b c) obj
           (setq a (make-instance <qqq>))
           (setq b (make-instance <qqq>))
           (setq c 42)
           (let ((ans nil))
             (~walkslots* <qqq>
                          (lambda (x) (push x ans))
                          (~mapslots* <qqq>
                                      #'1+
                                      obj))
             ans)))
       '(43 1 1 1 1 1 1)))
  (is (equalp
       (let ((obj (make-instance <qqq>)))
         (with-slots (a b c) obj
           (setq a (make-instance <qqq>))
           (setq b (make-instance <qqq>))
           (setq c 42)
           (let ((ans nil))
             (~walkslots* <qqq>
                          (lambda (x) (push x ans))
                          (~mapslots* <qqq> #'list obj))
             ans)))
       '((42) (0) (0) (0) (0) (0) (0))))
  (defmethod ~walkslots* ((class ~operating-class) 
                          (fn function)
                          (list list))
    (mapc fn list))
  (is (equalp
       (let ((obj (make-instance <qqq>)))
         (with-slots (a b c) obj
           (setq a (make-instance <qqq>))
           (setq b (make-instance <qqq>))
           (setq c 42)
           (let ((ans nil))
             (~walkslots* <qqq>
                          (lambda (x) (push x ans))
                          (~mapslots* <qqq> #'list obj))
             ans)))
       '(42 0 0 0 0 0 0)))
  (let ((method
         (find-method #'~walkslots*
                      nil
                      (list 
                       (find-class '~operating-class)
                       (find-class 'function)
                       (find-class 'list))
                      nil)))
    (when method
      (remove-method #'~walkslots* method))))


(test |attributed-class test|
  (~defclass @foo (~attributed-object)
    ((x :initform 1
        :attributes
        ((status :initform 'OK 
                 :attributes
                 ((meta-status :initform 'OK)))
         (explanation :initform nil) something-else))
     (y))
    (:default-attributes
     ((status :initform 'unknown))))
  (~defclass @bar (@foo)
    ((z))
    (:default-attributes
     ((zz :initform 'zzunknown))))
  (~defclass @baz (~attributed-object)
    ((x :initform 'x
        :attributes
        ((status :initform 'OK 
                 :attributes
                 ((meta-status :initform 'OK)))
         (explanation :initform (list (status ~self)
                                      (status ~self)))
         something-else))
     (y :initform (x ~self)))
    (:default-attributes
     ((status :initform 'unknown))))
  (let ((f (make-instance '@foo))
        (b (make-instance '@bar)))
    (is (eq 'ok (slot-value (~slot-attribute f 'x) 'status)))
    (is (eq 'zzunknown (~attribute-value b 'y 'zz)))
    (is (eq 'ok (~attribute-value b 'x 'status 'meta-status)))
    (is (equal '(ok ok)
               (~attribute-value (make-instance '@baz) 'x 'explanation)))))


(test |lazy-class test|
  (~defclass lazy-qqq (~lazy-object)
    ((a :initform 42 :initarg :a :initialization ((:at :read)))
     (b :initform 43 :initarg :b :initialization ((:at :access)))
     (c :initform 100 :initarg :c :initialization ((:after a)
                                                   (:at :access)))))
  (~defclass lazy-rrr (~lazy-object)
    (;; You can set it, or read it.
     (k :accessor k
        :initform 'k
        :initialization ((:at :read))
        )
     ;; You can set it, read it,
     ;; or initialize w (see below).
     (x :accessor x
        :initform 1
        :initialization ((:at :access))
        )
     ;; y0 cannot be accessed at all
     ;; before setting it explicitly.
     (y0 :accessor y0
         :initform (1+ (x ~self))
         :initialization ((:after x))) 
     ;; To use y, you must either set it, or initialize x.
     (y :accessor y
        :initform (1+ (x ~self))
        ) ;:initialization (:follows x)
     ;; To use z, you must either set it, or initialize y.
     ;; If you initialize w,
     ;; z won't get initialized if y is not.
     (z :accessor z
        :initform (1+ (y ~self))
        :initialization ((:check y) 
                         (:at :access)
                         (:before k))) 
     ;; You can set it, or read it.
     ;; If the :right-before argument was (x z),
     ;; initialization of w would always provoke z's.
     (w :accessor w
        :initform 3
        :initialization ((:at :access) (:implies z x))))
    )
  (is (equal (let ((o (make-instance 'lazy-qqq)))
               (list (incf (slot-value o 'b))
                     (slot-value o 'c)))
             '(43 100)))
  (is (equal (let* ((c (find-class 'lazy-rrr))
                    (i (make-instance c)))
               (mapcar (lambda (s)
                         (slot-value-using-class c i s))
                       (class-slots c)))
             
             '(k 1 2 2 3 3))))


;;; *EOF*
