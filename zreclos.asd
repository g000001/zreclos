;;;; zreclos.asd -*- Mode: Lisp;-*- 

(cl:in-package :asdf)


(defsystem :zreclos
  :serial t
  :depends-on (:fiveam
               :closer-mop)
  :components ((:file "package")
               #|(:file "readtable")|#
               (:file "metaclass-inheritance")
               (:file "zreclos")
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
