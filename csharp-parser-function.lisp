(ql:quickload "cl-utilities")
(in-package :csharp-parser)


(defun class-fn? (tokenizer node-stack)
  (with-peek
    (and (node-stack-has-visibility-node? node-stack)
	 (node-stack-has-type-node? node-stack)
	 (match peek "("))))

(defun parse-function-param-list (tokenizer)
  (parse-param-list tokenizer
		    "function-parameters"
		    #'(lambda (para-list)
			(mapcar #'(lambda (pairs)
				    (make-ast-node "function-parameter"
					  (list (make-ast-node "type" (car pairs))
					  (make-ast-node "variable-name" (cadr pairs)))))
				(code-generator-utils-space:make-pairs para-list)))))

(defun make-class-function (tokenizer node-stack)
    (destructuring-bind (visibility-node type-node) (read-vis-type node-stack) 
      (let ((fn-name (current-token tokenizer)))
	(advanze-token tokenizer)
	(advanze-token tokenizer)
	(let* ((param-pairs (parse-function-param-list tokenizer))
	       (function-ast (list visibility-node
				   type-node
				   (make-ast-node "function-name" fn-name)
				   param-pairs)))
	  (advanze-token tokenizer)
	  (advanze-token tokenizer)
	  (setq node-stack nil)
	  (parse-function-body tokenizer node-stack function-ast)
	  (setq node-stack nil)
	  (make-ast-node "function-node" function-ast)))))

(defun return-stmt? (tokenizer)
  (with-token 
    (if (match token "return")
	t
	nil)))

(defun make-return-node (tokenizer)
  (let ((return-node (make-ast-node "function-return" (parse-expression tokenizer))))
    return-node))

(defun make-function-variable (tokenizer node-stack)
  (make-variable tokenizer node-stack "function-variable"))

(defun parse-function-body (tokenizer node-stack ast-tree)
  (let ((function-call-node (make-instance 'function-call-ast-node)))
    (with-parse-stmts (tokenizer node-stack)
      ((return-stmt? tokenizer)
       (push-node (make-return-node tokenizer) ast-tree))
      ((variable? tokenizer)
       (push-node (make-function-variable tokenizer node-stack) ast-tree)
       (advanze-token tokenizer)
       (setq node-stack nil))
      ((function-call? tokenizer node-stack)
       (push-node (make-node function-call-node tokenizer node-stack) ast-tree)
       (setq node-stack nil)))))
