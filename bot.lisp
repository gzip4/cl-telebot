
(in-package :cl-telebot)


(defun make-api-url (method)
  (format nil "https://api.telegram.org/bot~A/~A"
	  (slot-value *bot* 'token) method))


(defun call-api-method (method &optional request)
  (let* ((content (and request (jonathan:to-json request)))
	 (response (dex:post (make-api-url method)
			     :keep-alive t
			     :headers '(("Content-Type" . "application/json"))
			     :content content)))
    (getf (jonathan:parse response) :|result|)))


(defun init-bot-params (bot params)
  (flet ((f (x) (if (char= x #\_) #\- x)))
    (let ((keys '(:|supports_inline_queries| :|can_read_all_group_messages|
		  :|can_join_groups| :|username| :|first_name| :|id|)))
      (loop :for key :in keys
	    :for slot-name = (string-upcase (map 'string #'f (string key)))
	    :for slot = (intern slot-name :cl-telebot)
	    :do (setf (slot-value bot slot) (getf params key)))))
  (when *debug* (describe bot))
  (values nil))


(defun skip-update (update)
  (setf (bot-update-id *bot*) (getf update :|update_id|)))


(defun process-update (update)
  (setf (bot-update-id *bot*) (getf update :|update_id|))
  (let ((*update-raw* update)
	(*update* (make-update update)))
    (on-update *bot* *update*)))


(defun process-updates (&optional skip)
  (let* ((update-id (bot-update-id *bot*)) request updates)
    (setf (getf request :|timeout|) (if skip 0 *timeout*))
    (when update-id
      (setf (getf request :|offset|) (1+ update-id)))
    (when *debug* (format *debug-io* "~&getUpdates: ~S~%" request))
    (setf updates (telebot::call-api-method "getUpdates" request))
    (mapcar (if skip #'skip-update #'process-update) updates)
    (sleep 0.00001)	    ; allows to interrupt (C-c C-c) processing
    updates))


(defun long-polling (bot &key debug)
  "TODO: doc"
  (let ((*bot* bot)
	(*debug* debug))
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
  "TODO: doc"
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
    (when *debug* (format *debug-io* "~&Send request: ~S~%" request))
    (let ((msg (make-message (call-api-method "sendMessage" request))))
      (when *debug* (format *debug-io* "~&Send msg: ~S~%" msg))
      (values msg))))


(defun reply (text &key parse-mode
		     disable-web-page-preview
		     disable-notification
		     reply-markup)
  "TODO: doc"
  (let ((message (or (getf *update-raw* :|message|)
		     (getf *update-raw* :|edited_message|))))
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


(defmethod on-update ((bot bot) update)
  "Default bot update handler"
  nil)


(defmethod on-update ((bot echo-bot) update)
  "Echo telegram bot"
  (let ((text (message-text (update-message update))))
    (when text
      (reply (format nil "echo bot: <i>~a</i>" text) :parse-mode :html))))
  
