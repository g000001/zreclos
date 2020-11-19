(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(defmetaclass ~ir-attributed-lazy-class
              (~ir-lazy-class ~ir-attributed-class ~attributed-lazy-class)
  ()
  (:metaclass standard-class))


;;; *EOF*
