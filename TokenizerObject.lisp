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
