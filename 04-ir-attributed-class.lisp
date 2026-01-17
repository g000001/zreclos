(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(defmetaclass ~ir-attributed-class (~attributed-class
                                    ~ir-self-referent-operating-class)
  ()
  (:metaclass eclos-class))


;;; *EOF*


