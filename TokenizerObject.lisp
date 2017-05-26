(in-package :tokenizer)

(defun current-token (tokenizer)
  (funcall tokenizer :current))

(defun advanze-token (tokenizer)
  (funcall tokenizer :advanze))

(defun peek-token (tokenizer)
  (funcall tokenizer :peek))

(defun tokenizer-object (token-list-org)
  (let ((token-list token-list-org))
    (flet ((current-token ()
	     (car token-list))
	   (peek-token ()
	     (cadr token-list))
	   (advanze-token ()
	     (setf token-list (cdr token-list))
	     (car token-list)))
    (lambda (cmd)
      (cond ((eq :current cmd) (current-token))
	    ((eq :peek cmd) (peek-token))
	    ((eq :advanze cmd) (advanze-token))
	    (t (current-token)))))))

(defun add-length-to-strings (string-list)
  (mapcar (lambda (x) (cons x (length x)))
	  string-list))

(defun tokenize-with-symbols (symbol-table string-to-tokenize)
  (tokenizer-object 
   (tokenize-fn string-to-tokenize
			  (add-length-to-strings symbol-table))))

(defmacro with-token-and-peek (&body body) 
  (let ((token (intern (symbol-name 'token)))
	(peek (intern (symbol-name 'peek)))
	(tokenizer (intern (symbol-name 'tokenizer))))
    `(let ((,token (current-token ,tokenizer))
	   (,peek (peek-token ,tokenizer)))
       ,@body)))

(defmacro with-token (&body body) 
  (let ((token (intern (symbol-name 'token)))
	(tokenizer (intern (symbol-name 'tokenizer))))
    `(let ((,token (current-token ,tokenizer)))
       ,@body)))
  
(defun print-tokens (tokenizer)
  (do ((token (current-token tokenizer) (advanze-token tokenizer))
       (x ()))
      ((funcall (lambda (x) (null (current-token x))) tokenizer) (nreverse x))
    (push token x)))

