;;;; srfi-40.asd

(cl:in-package :asdf)


(defsystem :srfi-40
  :version "20200324"
  :description "SRFI 40 for CL: A Library of Streams"
  :long-description "SRFI 40 for CL: A Library of Streams
https://srfi.schemers.org/srfi-40"
  :author "Philip L. Bewig"
  :maintainer "CHIBA Masaomi"
  :serial t
  :depends-on (:srfi-23
               :srfi-9
               :srfi-5
               :fiveam
               :mbe)
  :components ((:file "package")
               (:file "srfi-40")
               (:file "test")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-40))))
  (let ((name "https://github.com/g000001/srfi-40")
        (nickname :srfi-40))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-40))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/srfi-40#internals")))
    (eval
     (read-from-string
      "
      (or (let ((result (run 'srfi-40)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))


;;; *EOF*
