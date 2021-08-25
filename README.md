## Common Lisp Telegram Bot API

[![Quicklisp dist](http://quickdocs.org/badge/cl-telebot.svg)](http://quickdocs.org/cl-telebot/)

See the [Telegram Bot API](https://core.telegram.org/bots/api).

Code samples:
```lisp
;; Simple echo bot
(telebot:long-polling (make-instance 'telebot:echo-bot :token "<token>"))

;; Custom bot
(defclass my-bot (telebot:bot) ())
(defmethod telebot:on-update ((bot my-bot) update)
   (let ((text (telebot:message-text (telebot:update-message update))))
      ;; do something useful
      )
   
   (telebot:reply "some text")      ; reply current message, if any
   (telebot:send-message "<chat-id>" "some text")
   nil)
(telebot:long-polling (make-instance 'my-bot :token "<token>"))
```
