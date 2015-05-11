(in-package :cl-user)

(defpackage :spies-asd
  (:use :common-lisp :asdf))

(in-package :spies-asd)

(defsystem :spies
  :description "SPIRE (= Smart Parking for Intelligent Real Estate) Event Service"
  :version "1.0.0"
  :author "Mikko Rinne <mikko.rinne@aalto.fi>"
  :serial t
  :components ((:file "packages")
               (:file "spies_main")
	       (:file "camera-poll"))
  :depends-on (:hunchentoot :drakma :cl-json :cl-csv))
