;;;; Zillion Random Extentions of Common Lisp Object System
(cl:in-package zreclos.meta)

(in-syntax *zreclos-syntax*)

;;;;
;;;; Metaclass Inheritance: ECLOS 
;;;;

(defun build-transitive-closure (get-follow-ons)
  (lambda (x)
    (labels ((track (result pending)
               (if (endp pending)
                   result
                   (let ((next (car pending)))
                     (if (member next result)
                         (track result (cdr pending))
                         (track (cons next result)
                                (append (funcall get-follow-ons next)
                                        (cdr pending))))))))
      (track '() (list x)))))


(defun compute-metaclass (dsupers &key (default-metaclass-name nil))
  (block nil
    ;;Let C be a class, if
    ;;a) the definition of C includes a (:metaclass M) option then M is the metaclass of C.
    (when default-metaclass-name
      (return (find-class default-metaclass-name)))
    (when (endp dsupers)
      (return (find-class 'standard-class)))
    ;;b) let S be the set of direct superclasses of C 
    (let* ((| S | dsupers)
           (| M(S) | (mapcar #'class-of | S |))
           ;;and let M*(S) be the set of transitive closures of the subclass relation applied to the elements of M(S)
           (| M*(S) | (mapcar (build-transitive-closure #'class-direct-subclasses) | M(S) |))
           ;;and let T be the intersection of the sets composing M*(S)
           (| T | (reduce #'intersection | M*(S) |)))
      ;;then if T forms a tree according to the subclass relation 
      (if (and (not (null | T |))
               (every #'subtypep | T | (cdr | T |)))
          ;;then the root of T is the metaclass of C
          (car | T |)
          ;;otherwise STANDARD-CLASS is the metaclass of C.
          (find-class 'standard-class)))))


(defun ensure-class-soft (name)
  (or (find-class name nil)
      (make-instance 'standard-class :name name)))


#+lispworks
(defmacro ~defclass-eclos (name superclasses slots &rest class-options)
  (let* ((metaclass-name (cadr (find :metaclass class-options :key #'car)))
         (metaclass (compute-metaclass (mapcar #'ensure-class-soft superclasses)
                                       :default-metaclass-name metaclass-name))
         (metaclass (case (class-name metaclass)
                      (forward-referenced-class (find-class 'standard-class))
                      (otherwise metaclass))))
    (clos::expand-defclass (class-prototype metaclass)
                           (class-name metaclass)
                           name
                           superclasses
                           slots
                           class-options)))

;;;;
;;;; Metaclass Inheritance: STklos
;;;;

(let ((table-of-metas '()))
  (defun ensure-metaclass-with-supers (meta-supers)
    (let ((entry (assoc meta-supers table-of-metas :test #'equal)))
      (if entry
          ;; Found a previously created metaclass
          (cdr entry)
          ;; Create a new meta-class which inherit from "meta-supers"
          (let* ((name (mapcar #'class-name meta-supers))
                 (new (make-instance 'standard-class 
                                     :name name
                                     :direct-superclasses meta-supers
                                     :direct-slots '())))
            (setf (find-class name) new)
            (push (cons meta-supers new) table-of-metas)
            new)))))

(defun ensure-metaclass (supers)
  (if (endp supers)
      (find-class 'standard-class)
      (let* ((all-metas (mapcar #'class-of supers))
             (all-cpls  (mapcan (lambda (m)
                                  (copy-list (cdr (class-precedence-list m))))
                                all-metas))
             (needed-metas '()))
        ;; Find the most specific metaclasses.  The new metaclass will be
        ;; a subclass of these.
        (mapc
         (lambda (meta)
           (when (and (not (member meta all-cpls))
                      (not (member meta needed-metas)))
             (setq needed-metas (append needed-metas (list meta)))))
         all-metas)
        ;; Now return a subclass of the metaclasses we found.
        (if (endp (cdr needed-metas))
            (car needed-metas)  ; If there's only one, just use it.
            (ensure-metaclass-with-supers needed-metas)))))


#+lispworks
(defmacro ~defclass-stklos (name superclasses slots &rest class-options)
  (let* ((metaclass (ensure-metaclass (mapcar (lambda (s)
                                                (or (find-class s nil)
                                                    (make-instance 'standard-class :name s)))
                                              superclasses)))
         (metaclass (case (class-name metaclass)
                      (forward-referenced-class (find-class 'standard-class))
                      (otherwise metaclass))))
    (clos::expand-defclass (class-prototype metaclass)
                           (class-name metaclass)
                           name
                           superclasses
                           slots
                           class-options)))

#+lispworks
(defmacro ~defclass (name superclasses slots &rest class-options)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (,@(etypecase name
          (atom `(~defclass-eclos ,name))
          ((cons * (cons (eql :eclos) null))
           `(~defclass-eclos ,(car name)))
          ((cons * (cons (eql :stklos) null))
           `(~defclass-stklos ,(car name))))  
      ,superclasses
      ,slots
      ,@class-options)))

;;; *EOF*
