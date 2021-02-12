
(in-package :cl-telebot)

(defvar *bot* nil)
(defvar *update* nil)
(defvar *update-raw* nil)
(defvar *parse-mode* '(:markdown "Markdown" :html "HTML"))
(defvar *timeout* 1)
(defvar *debug* nil)
