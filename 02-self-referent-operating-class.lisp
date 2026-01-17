(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(defmetaclass ~self-referent-operating-class
              (~self-referent-class ~operating-class)
  ()
  (:slot-definitions-mixin-slots)
  (:metaclass eclos-class))
  


;;; *EOF*
