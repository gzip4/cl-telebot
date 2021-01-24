
(defpackage :cl-telebot
  (:use :common-lisp)
  (:nicknames :telebot)
  (:import-from :drakma :http-request)
  (:export #:bot
	   #:on-update
	   #:process-long-polling
	   #:send-message
	   #:reply
	   #:bot-supports-inline-queries
	   #:bot-can-read-all-group-messages
	   #:bot-can-join-groups
	   #:bot-username
	   #:bot-first-name
	   #:bot-id
	   #:bot-update-id))

