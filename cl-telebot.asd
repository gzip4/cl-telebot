
(defpackage :cl-telebot-system
  (:use :common-lisp))

(in-package :cl-telebot-system)

(asdf:defsystem :cl-telebot
  :serial t
  :version "0.2.0"
  :description "Common Lisp Telegram Bot API"
  :author "gzip4 <gzip4ever@gmail.com>"
  :license "MIT"
  :depends-on (:dexador :jonathan :alexandria)
  :components ((:file "packages")
               (:file "specials")
               (:file "conditions")
               (:file "classes")
               (:file "objects")
               (:file "bot")))

