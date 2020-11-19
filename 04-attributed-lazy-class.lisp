(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(defmetaclass ~attributed-lazy-class
              (~lazy-class ~attributed-class)
  ()
  (:metaclass standard-class))


;;; *EOF*
