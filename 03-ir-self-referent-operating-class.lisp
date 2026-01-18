(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(~defmetaclass ~ir-self-referent-operating-class 
              (~instance-recording-class ~self-referent-class ~operating-class)
  ()
  (:metaclass eclos-class))


;;; *EOF*
