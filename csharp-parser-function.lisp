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
	  (make-ast-node "function-node" function-ast)))))

(defun return-stmt? (tokenizer)
  (with-token 
    (if (match token "return")
	t
	nil)))

(defun make-function-variable (tokenizer node-stack)
  (make-variable tokenizer node-stack "function-variable"))

(defun make-expression-node (tokenizer node-stack)
  (advanze-token tokenizer)
  (let ((return-node (make-ast-node "function-return" (list (expression tokenizer node-stack)))))
    return-node))

(defun parse-function-body (tokenizer node-stack ast-tree)
  (with-parse-stmts (tokenizer node-stack)
    ((return-stmt? tokenizer)
     (push-node (make-expression-node tokenizer node-stack) ast-tree))
    ((variable? tokenizer)
     (push-node (make-function-variable tokenizer node-stack) ast-tree)
     (setq node-stack nil)
     (advanze-token tokenizer))))
