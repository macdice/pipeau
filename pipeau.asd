(defpackage :pipeau-asd
  (:use :cl :asdf))

(in-package :pipeau-asd)

(defsystem pipeau
  :name "Pipeau"
  :version "0.0.0"
  :description "New experiments in concurrent programming."
  :serial t
  :components ((:file "package")
               (:file "mailbox")
               (:file "match")
               (:file "process"))
   :depends-on (:bordeaux-threads))
