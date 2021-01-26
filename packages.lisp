
(in-package :common-lisp-user)

(defpackage :cl-telebot
  (:use :common-lisp)
  (:nicknames :telebot)
  (:export #:bot
	   #:echo-bot
	   #:on-update
	   #:long-polling
	   #:send-message
	   #:reply
	   #:bot-supports-inline-queries
	   #:bot-can-read-all-group-messages
	   #:bot-can-join-groups
	   #:bot-username
	   #:bot-first-name
	   #:bot-id
	   #:bot-update-id
	   #:*debug*
	   #:*timeout*

	   #:entity
	   #:entity-type
	   #:entity-offset
	   #:entity-length
	   #:entity-url
	   #:object
	   #:object-id
	   #:user
	   #:user-first-name
	   #:user-last-name
	   #:user-username
	   #:user-language-code
	   #:user-is-bot
	   #:chat
	   #:chat-type
	   #:chat-title
	   #:chat-username
	   #:chat-first-name
	   #:chat-last-name
	   #:chat-all-members-are-administrators
	   #:message
	   #:message-from
	   #:message-date
	   #:message-chat
	   #:message-forward-from
	   #:message-forward-date
	   #:message-reply-to-message
	   #:message-edit-date
	   #:message-text
	   #:message-entities
	   #:message-audio
	   #:message-document
	   #:message-photo
	   #:message-sticker
	   #:message-video
	   #:message-voice
	   #:message-caption
	   #:message-contact
	   #:message-location
	   #:message-venue
	   #:message-new-chat-member
	   #:message-left-chat-member
	   #:message-new-chat-title
	   #:message-new-chat-photo
	   #:message-delete-chat-photo
	   #:message-group-chat-created
	   #:message-supergroup-chat-created
	   #:message-channel-chat-created
	   #:message-migrate-to-chat-id
	   #:message-migrate-from-chat-id
	   #:message-pinned-message
	   #:update
	   #:update-message
	   #:update-inline-query
	   #:update-chosen-inline-result
	   #:update-callback-query))

