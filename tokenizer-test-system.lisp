(in-package :tokenizer)

(defun match-test-string (token control-test-string)
  (string= token control-test-string))

(defun match-token-list (tokenizer control-token-list)
  (equal t
	 (let ((is-matching t))
	   (dolist (control-token control-token-list)
	     (if (match-test-string (current-token tokenizer)
				    control-token)
		 t
		 (setf is-matching nil))
	     (advanze-token tokenizer))
	   is-matching)))

