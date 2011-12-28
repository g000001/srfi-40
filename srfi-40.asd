;;;; srfi-40.asd

(cl:in-package :asdf)

(defsystem :srfi-40
  :serial t
  :depends-on (:srfi-23
               :srfi-9
               :srfi-5
               :fiveam
;               :srfi-45
               :mbe)
  :components ((:file "package")
               (:file "srfi-40")
               (:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-40))))
  (load-system :srfi-40)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-40.internal :srfi-40))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))
