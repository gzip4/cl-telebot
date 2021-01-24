
(in-package :cl-telebot)


(defvar *bot* nil)
(defvar *accept* "application/json")
(defvar *update* nil)
(defvar *parse-mode* '(:markdown "Markdown" :html "HTML"))


(defclass bot ()
  ((token :initarg :token)
   (supports-inline-queries :reader bot-supports-inline-queries)
   (can-read-all-group-messages :reader bot-can-read-all-group-messages)
   (can-join-groups :reader bot-can-join-groups)
   (username :reader bot-username)
   (first-name :reader bot-first-name)
   (id :reader bot-id)
   (update-id :initform nil :accessor bot-update-id)))


(defgeneric on-update (bot update))


(defmethod on-update ((bot bot) update) nil)
  

(defun make-api-url (method)
  (format nil "https://api.telegram.org/bot~A/~A"
	  (slot-value *bot* 'token) method))


(defun call-api-method (method &optional request)
  (let* ((content (and request (jonathan:to-json request)))
	 (response (dex:post (make-api-url method)
			     :headers '(("Content-Type" . "application/json"))
			     :content content)))
    (getf (jonathan:parse response) :|result|)))


;; bad version, consumes cpu on wait
(defun call-api-method2 (method &optional request)
  (let* ((content (and request (jonathan:to-json request)))
	 (response (http-request (make-api-url method)
				 :method :post
				 :content-type *accept*
				 :accept *accept*
				 :content content))
	 (json-string (flex:octets-to-string response :external-format :utf-8)))
    (getf (jonathan:parse json-string) :|result|)))


(defun init-bot-params (bot params)
  (setf (slot-value bot 'supports-inline-queries) (getf params :|supports_inline_queries|)
	(slot-value bot 'can-read-all-group-messages) (getf params :|can_read_all_group_messages|)
	(slot-value bot 'can-join-groups) (getf params :|can_join_groups|)
	(slot-value bot 'username) (getf params :|username|)
	(slot-value bot 'first-name) (getf params :|first_name|)
	(slot-value bot 'id) (getf params :|id|)))


(defun skip-update (update)
  (setf (bot-update-id *bot*) (getf update :|update_id|)))


(defun process-update (update)
  (setf (bot-update-id *bot*) (getf update :|update_id|))
  (let ((*update* update))
    (on-update *bot* update)))


(defun process-updates (&optional skip)
  (let* ((update-id (bot-update-id *bot*))
	 (request (and update-id (list :|offset| (1+ update-id) :|timeout| 1)))
	 (updates (telebot::call-api-method "getUpdates" request)))
    (mapcar (if skip #'skip-update #'process-update) updates)
    updates))


(defun process-long-polling (bot)
  (let ((*bot* bot)
	(drakma:*drakma-default-external-format* :utf-8))
    (init-bot-params bot (call-api-method "getMe"))
    (loop :for updates = (process-updates :skip)
	  :while updates :do (progn))
    (loop
      (handler-case (process-updates)
	(error (x) (format *error-output* "ERROR: ~A" x))))))


(defun send-message (chat-id text &key (parse-mode :markdown)
				    disable-web-page-preview
				    disable-notification
				    reply-to-message-id
				    reply-markup)
  (declare (ignorable reply-markup))
  (let (request)
    (setf (getf request :|chat_id|) chat-id
	  (getf request :|text|) text
	  (getf request :|parse_mode|) (getf *parse-mode* parse-mode "Markdown"))
    (when reply-to-message-id
      (setf (getf request :|reply_to_message_id|) reply-to-message-id))
    (when disable-web-page-preview
      (setf (getf request :|disable_web_page_preview|) t))
    (when disable-notification
      (setf (getf request :|disable_notification|) t))
    (format t "~A: ~A~&" "send" request)
    (telebot::call-api-method "sendMessage" request)))


(defun reply (text &key (parse-mode :markdown)
		     disable-web-page-preview
		     disable-notification
		     reply-markup)
  (let ((message (getf *update* :|message|)))
    (when message
      (let* ((message-id (getf message :|message_id|))
	     (chat (getf message :|chat|))
	     (chat-id (getf chat :|id|))
	     (chat-type (getf chat :|type| "unknown"))
	     (reply-to (if (string/= chat-type "private") message-id)))
	(when chat-id
	  (send-message chat-id text
			:parse-mode parse-mode
			:disable-web-page-preview disable-web-page-preview
			:disable-notification disable-notification
			:reply-to-message-id reply-to
			:reply-markup reply-markup))))))
