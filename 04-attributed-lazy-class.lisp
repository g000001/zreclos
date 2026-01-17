(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(defmetaclass ~attributed-lazy-class
              (~attributed-class ~lazy-class)
  ()
  (:slot-definitions-mixin-slots)
  (:direct-slot-definitions-mixin-slots)
  (:effective-slot-definitions-mixin-slots)
  (:metaclass eclos-class))

;;; *EOF*
