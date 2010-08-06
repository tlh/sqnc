(in-package :cl-user)

(defpackage #:csound-asd (:use :cl :asdf))

(in-package :csound-asd)

(defsystem csound
  :name "csound"
  :version "0.0.0"
  :author "Michael Boulanger?"
  :components ((:file "csound"))
  :depends-on (:cffi))

