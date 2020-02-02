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
	(let ((function-param-node (parse-function-param-list tokenizer))
	      (function-node (make-ast-node "function-node" ()))
	      (function-declaration-node (make-ast-node "function-declaration" ()))
	      (function-name-node (make-ast-node "function-name" fn-name))
	      (function-body-node (make-ast-node "function-body" ())))
	        ;;(list visibility-node
		;;		   type-node
		;;		   (make-ast-node "function-name" fn-name)
		;;;		   param-pairs)))
	  (advanze-token tokenizer)
	  (advanze-token tokenizer)
	  (push-node visibility-node function-declaration-node)
	  (push-node function-name-node function-declaration-node)
	  (push-node function-param-node function-declaration-node)
	  (push-node function-declaration-node function-node)
	  (setq node-stack nil)
	  (parse-function-body tokenizer node-stack function-body-node)
	  (setq node-stack nil)
	  (push-node function-body-node function-node)
	  function-node))))

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
  (with-parse-stmts (tokenizer node-stack)
    ((return-stmt? tokenizer)
     (push-node (make-return-node tokenizer) ast-tree)
     (setq node-stack nil)
     (advanze-token tokenizer))
    ((variable? tokenizer)
     (push-node (make-function-variable tokenizer node-stack) ast-tree)
     (setq node-stack nil)
     (advanze-token tokenizer))
    ((function-call? tokenizer node-stack)
     (push-node (make-function-call-node tokenizer node-stack) ast-tree)
     (setq node-stack nil)
     (advanze-token tokenizer))))
