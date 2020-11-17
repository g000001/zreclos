;;;; zreclos.lisp -*- Mode: Lisp;-*- 
(cl:in-package :zreclos.meta)
(in-syntax *zreclos-syntax*)


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

(defun standard-instance-wrapper (ins)
  #+allegro (excl::std-instance-wrapper ins)
  #+lispworks (clos::standard-instance-wrapper ins)
  #+sbcl (sb-kernel::%instance-layout ins)
  #+ccl (ccl::instance.class-wrapper ins))


(defun standard-instance-slots (ins)
  #+allegro (excl::std-instance-slots ins)
  #+lispworks (clos::standard-instance-static-slots ins)
  #+sbcl (sb-pcl::std-instance-slots ins)
  #+ccl (ccl::instance.slots ins))


#|(standard-instance-wrapper
 (alloc-fix-instance (class-wrapper (find-class 'standard-object))
                     #(a b c)))

(c2mop:standard-instance-access
 (alloc-fix-instance (class-wrapper (find-class 'standard-object))
                     #(a b c))
 0)

(standard-instance-slots
 (alloc-fix-instance (class-wrapper (find-class 'standard-object))
                     '(a b c)))|#

#|;



(defclass ~self-referent-operating-object 
          (~operating-object ~self-referent-object)
  ()
  (:metaclass ~self-referent-operating-class))


'(defclass ~lazy-class
          (~self-referent-operating-class)
  ()
  (:metaclass standard-class))


'(defclass ~attributed-class
          (~self-referent-operating-class)
  ()
  (:metaclass standard-class))


'(defclass ~attributed-lazy-class
          (~attributed-class
           ~lazy-class)
  ()
  (:metaclass standard-class))


'(defclass ~constrained-class
          (~attributed-class)
  ()
  (:metaclass standard-class))


'(defclass ~constrained-lazy-class
          (~constrained-class
           ~lazy-class)
  ()
  (:metaclass standard-class))


(defclass ~ir-operating-class
          (~instance-recording-class
           ~operating-class)
  ()
  (:metaclass standard-class))

(defclass ~ir-operating-object
          (~instance-recording-object
           ~operating-object)
  ()
  (:metaclass ~ir-operating-class))


(defclass ~ir-self-referent-operating-class
          (~instance-recording-class 
           ~self-referent-operating-class
           ~operating-class)
  ()
  (:metaclass standard-class))


(defclass ~ir-self-referent-operating-object
          (~instance-recording-object 
           ~self-referent-operating-object
           ~operating-object)
  ()
  (:metaclass ~ir-self-referent-operating-class))


'(defclass ~ir-lazy-class
          (~ir-self-referent-operating-class
           ~lazy-class)
  ()
  (:metaclass standard-class))


'(defclass ~ir-attributed-lazy-class
          (~ir-attributed-class
           ~attributed-lazy-class
           ~ir-lazy-class)
  ()
  (:metaclass standard-class))


'(defclass ~ir-attributed-class
          (~instance-recording-class
           ~attributed-class)
  ()
  (:metaclass standard-class))


'(defclass ~ir-constrained-class
          (~instance-recording-class
           ~constrained-class)
  ()
  (:metaclass standard-class))


'(defclass ~ir-constrained-lazy-class
          (~ir-constrained-class
           ~constrained-lazy-class
           ~ir-attributed-lazy-class)
  ()
  (:metaclass standard-class))

;|#
;;; *EOF*
