
(in-package :cl-telebot)


(defmethod print-object ((obj entity) stream)
  (print-unreadable-object (obj stream :type nil :identity nil)
    (format stream "ENTITY:type=~s,text=~s,offset=~a,length=~a~@[,url=~a~]"
	    (entity-type obj)
	    (entity-text obj)
	    (entity-offset obj)
	    (entity-length obj)
	    (entity-url obj))))

(defmethod print-object ((obj user) stream)
  (print-unreadable-object (obj stream :type nil :identity nil)
    (format stream "USER:id=~a,first-name=~a~@[,last-name=~a~]~@[,username=~a~]~@[,language-code=~a~]~@[,is-bot=~a~]"
	    (object-id obj)
	    (user-first-name obj)
	    (user-last-name obj)
	    (user-username obj)
	    (user-language-code obj)
	    (user-is-bot obj))))

(defmethod print-object ((obj chat) stream)
  (print-unreadable-object (obj stream :type nil :identity nil)
    (format stream "CHAT:id=~a,type=~a~@[,title=~a~]~@[,username=~a~]~@[,first-name=~a~]~@[,last-name=~a~]"
	    (object-id obj)
	    (chat-type obj)
	    (chat-title obj)
	    (chat-username obj)
	    (chat-first-name obj)
	    (chat-last-name obj))))

(defmethod print-object ((obj message) stream)
  (print-unreadable-object (obj stream :type nil :identity nil)
    (format stream "MESSAGE:id=~a,date=~a,chat=~a~@[,from=~a~]~@[,text=~s~]~@[,edit-date=~a~]~@[,entities=(~{~a~^ ~})~]"
	    (object-id obj)
	    (message-date obj)
	    (message-chat obj)
	    (message-from obj)
	    (message-text obj)
	    (message-edit-date obj)
	    (message-entities obj))))

(defmethod print-object ((obj update) stream)
  (print-unreadable-object (obj stream :type nil :identity nil)
    (format stream "UPDATE:id=~a~@[,message=~a~]~@[,inline-query=~a~]~@[,chosen-inline-result=~a~]~@[,callback-query=~a~]"
	    (object-id obj)
	    (update-message obj)
	    (update-inline-query obj)
	    (update-chosen-inline-result obj)
	    (update-callback-query obj)
	    )))


(defun make-entity (text entity)
  (when entity
    (flet ((f (x) (if (char= x #\_) #\- x)))
      (let ((e (make-instance 'entity))
	    (type (string-upcase (map 'string #'f (getf entity :|type|))))
	    (offset (getf entity :|offset|))
	    (length (getf entity :|length|)))
	(setf (slot-value e 'type) (alexandria:make-keyword type)
	      (slot-value e 'offset) offset
	      (slot-value e 'length) length
	      (slot-value e 'url) (getf entity :|url|)
	      (slot-value e 'text) (subseq text offset (+ offset length)))
	(values e)))))


(defun make-entities (entities text)
  (mapcar (alexandria:curry #'make-entity text) entities))


(defun make-user (user)
  (when user
    (let ((u (make-instance 'user :id (getf user :|id|))))
      (setf (slot-value u 'first-name) (getf user :|first_name|)
	    (slot-value u 'last-name) (getf user :|last_name|)
	    (slot-value u 'username) (getf user :|username|)
	    (slot-value u 'language-code) (getf user :|language_code|)
	    (slot-value u 'is-bot) (getf user :|is_bot|))
      (values u))))


(defun make-chat (chat)
  (when chat
    (let ((c (make-instance 'chat :id (getf chat :|id|))))
      (setf (slot-value c 'type) (alexandria:make-keyword (getf chat :|type|))
	    (slot-value c 'title) (getf chat :|title|)
	    (slot-value c 'username) (getf chat :|username|)
	    (slot-value c 'first-name) (getf chat :|first_name|)
	    (slot-value c 'last-name) (getf chat :|last_name|)
	    (slot-value c 'all-members-are-administrators) (getf chat :|all_members_are_administrators|))
      (values c))))


(defun make-message (message)
  (when message
    (let ((m (make-instance 'message :id (getf message :|message_id|))))
      (setf (slot-value m 'date) (getf message :|date|)
	    (slot-value m 'chat) (make-chat (getf message :|chat|))
	    (slot-value m 'from) (make-user (getf message :|from|))
	    (slot-value m 'edit-date) (getf message :|edit_date|)
	    (slot-value m 'text) (getf message :|text|)
	    (slot-value m 'forward-from) (make-user (getf message :|forward_from|))
	    (slot-value m 'forward-date) (getf message :|forward_date|)
	    (slot-value m 'reply-to-message) (make-message (getf message :|reply_to_message|))
	    (slot-value m 'entities) (make-entities (getf message :|entities|) (slot-value m 'text))
	    (slot-value m 'audio) (getf message :|audio|)
	    (slot-value m 'document) (getf message :|document|)
	    (slot-value m 'photo) (getf message :|photo|)
	    (slot-value m 'sticker) (getf message :|sticker|)
	    (slot-value m 'video) (getf message :|video|)
	    (slot-value m 'voice) (getf message :|voice|)
	    (slot-value m 'caption) (getf message :|caption|)
	    (slot-value m 'contact) (getf message :|contact|)
	    (slot-value m 'location) (getf message :||)
	    (slot-value m 'venue) (getf message :|venue|)
	    (slot-value m 'new-chat-member) (make-user (getf message :|new_chat_member|))
	    (slot-value m 'left-chat-member) (make-user (getf message :|left_chat_member|))
	    (slot-value m 'new-chat-title) (getf message :|new_chat_title|)
	    (slot-value m 'new-chat-photo) (getf message :|new_chat_photo|)
	    (slot-value m 'delete-chat-photo) (getf message :|delete_chat_photo|)
	    (slot-value m 'group-chat-created) (getf message :|group_chat_created|)
	    (slot-value m 'supergroup-chat-created) (getf message :|supergroup_chat_created|)
	    (slot-value m 'channel-chat-created) (getf message :|channel_chat_created|)
	    (slot-value m 'migrate-to-chat-id) (getf message :|migrate_to_chat_id|)
	    (slot-value m 'migrate-from-chat-id) (getf message :|migrate_from_chat_id|)
	    (slot-value m 'pinned-message) (make-message (getf message :|pinned_message|)))
      (values m))))


(defun make-update (update)
  (when update
    (let ((u (make-instance 'update :id (getf update :|update_id|)))
	  (msg (or (getf update :|message|)
		   (getf update :|edited_message|))))
      (setf (slot-value u 'message) (make-message msg))
      (values u))))

;; allows, i.e. (message-from (update-message update)) -> NIL
(defmethod object-id ((object null)) nil)
(defmethod message-from ((object null)) nil)
(defmethod message-forward-from ((object null)) nil)
(defmethod message-reply-to-message ((object null)) nil)
(defmethod message-edit-date ((object null)) nil)
(defmethod message-text ((object null)) nil)
(defmethod message-entities ((object null)) nil)
(defmethod message-new-chat-member ((object null)) nil)
(defmethod message-left-chat-member ((object null)) nil)
(defmethod message-new-chat-title ((object null)) nil)
(defmethod message-new-chat-photo ((object null)) nil)
(defmethod message-delete-chat-photo ((object null)) nil)
(defmethod message-group-chat-created ((object null)) nil)
(defmethod message-supergroup-chat-created ((object null)) nil)
(defmethod message-channel-chat-created ((object null)) nil)
(defmethod message-migrate-to-chat-id ((object null)) nil)
(defmethod message-migrate-from-chat-id ((object null)) nil)
(defmethod message-pinned-message ((object null)) nil)

