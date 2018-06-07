(ql:quickload "cl-utilities")
(in-package :csharp-parser)

(defclass function-call-ast-node (ast-node-space:ast-node-base-class)())

(defmethod is-node-type? ((node function-call-ast-node) tokenizer node-stack)
  (with-token-and-peek
      (and (is-identifier? token)
	   (match-para-begin peek))))

(defun parse-function-call-arguments (tokenizer)
  (labels ((parse-arguments (tokenizer expression-node-list)
	     (if (match-para-end (current-token tokenizer))
		 expression-node-list
		 (progn
		   (push (parse-expression tokenizer) expression-node-list)
		   (parse-arguments tokenizer expression-node-list)))))
    (nreverse (parse-arguments tokenizer ()))))

(defmethod make-node ((node function-call-ast-node) tokenizer node-stack)
  (let ((fn-name (make-ast-node "function-call-name" (current-token tokenizer))))
    (advanze-token tokenizer)
    (let* ((params (make-ast-node "function-arguments" (parse-function-call-arguments tokenizer)))
	  (function-params-ast (list fn-name
				     params)))
      (make-ast-node "function-call" function-params-ast))))
