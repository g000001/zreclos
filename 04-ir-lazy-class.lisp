(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(defmetaclass ~ir-lazy-class
              (~ir-self-referent-operating-class ~lazy-class)
  ()
  (:metaclass eclos-class))


;;; *EOF*
