(ql:quickload "cl-utilities")
(in-package :csharp-parser)

(defun class-function-call? (tokenizer)
  (with-token-and-peek
      (and (is-identifier? token)
	   (match-para-begin peek))))

(defun make-function-call (tokenizer)
  (let ((fn-name (make-ast-node "function-call-name" (current-token tokenizer))))
    (advanze-token tokenizer)
    (advanze-token tokenizer)
    (let* ((params (parse-param-list tokenizer "function-call-params"))
	  (function-params-ast (list fn-name
				     params)))
      (make-ast-node "function-call" function-params-ast))))
