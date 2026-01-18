(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(~defmetaclass ~ir-operating-class (~instance-recording-class ~operating-class)
  ()
  (:metaclass eclos-class))


;;; *EOF*
