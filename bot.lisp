
(in-package :cl-telebot)


(defclass bot ()
  ((token :initarg :token)))


(defgeneric on-get-me (bot info))
(defgeneric on-update (bot update))


(defmethod on-get-me ((bot bot) info)
  nil)
(defmethod on-update ((bot bot) update)
  nil)


