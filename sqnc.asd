; -*- Mode: Lisp -*-

(defpackage #:sqnc-asd (:use :cl :asdf))

(in-package #:sqnc-asd)

(defsystem sqnc
  :name       "sqnc"
  :author     "tlh"
  :license    "MIT license"
  :version    "0.0.7"
  :serial     t
  :components ((:file "csnd")
               (:file "util")
               (:file "defs")
               (:file "clcs")
               (:file "colors")
               (:file "disp")
               (:file "keys")
               (:file "core")
               (:file "cmds")
               (:file "view")
               (:file "bind"))
  :depends-on (:cffi :cl-ncurses :cl-store :swank))
