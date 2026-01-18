;;;; zreclos.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)


(defsystem :zreclos
  :serial t
  :depends-on (:fiveam
               :closer-mop
               :series)
  :components ((:file "package")
               (:file "dsl")
               (:file "metaclass-inheritance")
               (:file "zreclos")
               (:file "required-slot-class")
               (:file "accessor-prefix-class")
               (:file "00-standard-class")
               (:file "01-instance-recording-class")
               (:file "01-operating-class")
               #+lispworks 
               (:file "01-self-referent-class")
               (:file "02-ir-operating-class")
               (:file "02-self-referent-operating-class")
               (:file "03-attributed-class")
               (:file "031-lazy-class")
               (:file "03-ir-self-referent-operating-class")
               (:file "04-attributed-lazy-class")
               (:file "04-ir-attributed-class")
               (:file "04-ir-lazy-class")
               (:file "05-ir-attributed-lazy-class")
               (:file "faceted-slot-class")
               (:file "test")))


(defmethod perform ((o test-op) (c (eql (find-system :zreclos))))
  (load-system :zreclos)
  (or (flet (($ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
        (let ((result (funcall ($ :fiveam :run) ($ :zreclos.meta :zreclos))))
          (funcall ($ :fiveam :explain!) result)
          (funcall ($ :fiveam :results-status) result)))
      (error "test-op failed") ))


;;; *EOF*
