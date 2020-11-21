(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(defmetaclass ~self-referent-operating-class
              (~operating-class
               ~self-referent-class)
  ()
  (:slot-definitions-mixin-slots)
  (:metaclass standard-class))
  


;;; *EOF*
