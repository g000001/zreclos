(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(defmetaclass ~attributed-lazy-class
              (~lazy-class ~attributed-class)
  ()
  (:slot-definitions-mixin-slots)
  (:direct-slot-definitions-mixin-slots)
  (:effective-slot-definitions-mixin-slots)
  (:metaclass standard-class))

;;; *EOF*
