(ql:quickload "cl-utilities")
(in-package :csharp-parser)

(defun variable? (tokenizer)
  (with-peek
    (or (match-assign peek) 
	(match-end peek))))

(defun make-variable (tokenizer node-stack enclosing-node-name)
  (if (not (variable? tokenizer))
      (print "error parsing variable"))
  (destructuring-bind (visibility-node type-node) (read-vis-type node-stack) 
    (let* ((name (current-token tokenizer))
	   (peek (peek-token tokenizer))
	   (value (if (match-assign peek)
		      (progn
			(advanze-token tokenizer)
			(parse-expression tokenizer))
		      nil)))
      (make-ast-node enclosing-node-name
		     (list visibility-node
			   (make-ast-node "variable-name" name)
			   type-node
			   (make-ast-node "variable-value" value))))))
