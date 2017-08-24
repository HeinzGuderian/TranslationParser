(in-package :tokenizer)

(define-condition token-mismatch-error (error)
  ((text :initarg :text :reader text)))

(defun match-test-string (token control-test-string)
  (string= token control-test-string))

(defun match-token-list (tokenizer control-token-list)
  (equal t
	 (let ((is-matching t)
	       (index 0))
	   (dolist (control-token control-token-list)
	     (if (match-test-string (current-token tokenizer)
				    control-token)
		 t
		 (progn (setf is-matching nil)
			(error 'token-mismatch-error
			       :text (concatenate 'string "Wrong token: '" (current-token tokenizer)
						  "' Correct token: '" control-token
						  "' Place correct token: " (write-to-string index)))))
	     (setf index (+ 1 index))
	     (advanze-token tokenizer))
	   is-matching)))

