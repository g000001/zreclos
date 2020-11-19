(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(defgeneric ultimate-ancestor-object-class-given-metaclass (class))


(defmethod ultimate-ancestor-object-class-given-metaclass ((class (eql 'standard-class)))
  'standard-object)


(defmacro defmetaclass (name (&rest supers) (&rest slots) &rest opts)
  (let* ((prefix (remove-\"class\"-suffix name))
         (default-supers (or supers '(standard-class)))
         (defeslotds (find :effective-slot-definitions-mixin-slots
                           opts :key #'car))
         (defdslotds (find :direct-slot-definitions-mixin-slots opts
                           :key #'car))
         (defslotds (find :slot-definitions-mixin-slots opts
                          :key #'car))
         (newopts (mapcan (lambda (o)
                            (case (car o)
                              ((:effective-slot-definitions-mixin-slots
                                :direct-slot-definitions-mixin-slots
                                :slot-definitions-mixin-slots)
                               nil)
                              (otherwise (list o))))
                          opts))
         (slotd  (symbolconc prefix '-slot-definition))
         (dslotd (symbolconc prefix '-direct-slot-definition))
         (eslotd (symbolconc prefix '-effective-slot-definition)))
    `(progn
       (finalize-inheritance
        (~defclass ,name (,@default-supers)
          ,slots
          ,@newopts))
       ,@(mapcar (lambda (s)
                   `(defmethod validate-superclass ((class ,name)
                                                    (super ,s))
                      T))
                 default-supers)
       (defmethod ultimate-ancestor-object-class-given-metaclass 
                  ((class (eql ',name)))
         (find-class ',(symbolconc prefix '-object)))
       (finalize-inheritance
        (ensure-class ',(symbolconc prefix '-object)
                      :direct-superclasses 
                      (mapcar #'ultimate-ancestor-object-class-given-metaclass ',default-supers)
                      :metaclass ',name))
       ,@(and 
          defslotds
          `((finalize-inheritance
             (defclass ,slotd (,@(mapcar (lambda (s)
                                           (class-name 
                                            (effective-slot-definition-class
                                             (class-prototype (find-class s)))))
                                         default-supers)) 
               ,@(and defslotds (list (cdr defslotds)))))))
       ,@(and 
          defdslotds
          `((finalize-inheritance
             (defclass ,dslotd (,@(mapcar (lambda (s)
                                            (class-name 
                                             (direct-slot-definition-class
                                              (class-prototype (find-class s)))))
                                          default-supers)) 
               ,@(and defdslotds (list (cdr defdslotds)))))
            (defmethod direct-slot-definition-class ((class ,name)
                                                     &rest initargs)
              (declare (ignore initargs))
              (find-class ',dslotd))))
       ,@(and
          defeslotds
          `((finalize-inheritance
             (defclass ,eslotd (,@(mapcar (lambda (s)
                                            (class-name 
                                             (effective-slot-definition-class
                                              (class-prototype (find-class s)))))
                                          default-supers)) 
               ,@(and defeslotds (list (cdr defeslotds)))))
            (defmethod effective-slot-definition-class ((class ,name)
                                                        &rest initargs)
              (declare (ignore initargs))
              (find-class ',eslotd)))))))


;;; *EOF*
