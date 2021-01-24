
(defpackage :cl-telebot-system
  (:use :common-lisp))

(in-package :cl-telebot-system)

(asdf:defsystem :cl-telebot
  :serial t
  :version "0.1.0"
  :depends-on (:drakma :dexador :jonathan :flexi-streams)
  :components ((:file "packages")
               (:file "conditions")
               (:file "bot")))

