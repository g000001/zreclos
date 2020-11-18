(cl:in-package zreclos.meta)


(in-syntax *zreclos-syntax*)


(defmetaclass ~ir-self-referent-operating-class 
              (~instance-recording-class ~operating-class ~self-referent-class)
  ()
  (:metaclass standard-class))


;;; *EOF*
