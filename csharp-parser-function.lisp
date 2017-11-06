(ql:quickload "cl-utilities")
(in-package :csharp-parser)


(defun return-stmt? (tokenizer node-stack)
  (with-token 
    (if (match token "return")
	t
	nil)))

(defun make-function-variable (tokenizer node-stack)
  (make-variable tokenizer node-stack "function-variable"))

(defun parse-function-body (tokenizer node-stack ast-tree)
  (with-parse-stmts (tokenizer node-stack)
    ((return-stmt? tokenizer node-stack)
     (push-node (make-expression-node tokenizer node-stack) ast-tree))
    ((variable? tokenizer node-stack)
     (push-node (make-function-variable tokenizer node-stack) ast-tree)
     (setq node-stack nil)
     (advanze-token tokenizer))))
