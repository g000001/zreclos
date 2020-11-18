(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(defmetaclass ~ir-attributed-class (~ir-self-referent-operating-class
                                    ~attributed-class)
  ()
  (:metaclass standard-class))


;;; *EOF*
