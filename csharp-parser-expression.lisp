(ql:quickload "cl-utilities")
(ql:quickload "alexandria")
(in-package :csharp-parser)

(defmacro with-parse-expression ((tokenizer) &body body)
  `(loop do
	(progn (advanze-token ,tokenizer)
	       (with-token-and-peek ,@body
	       ))
      while (and (not (eq (peek-token ,tokenizer) nil))
		 (not (match-end (peek-token ,tokenizer))))))

(defun is-nested? (token)
  (match-para-begin token))

(defun is-identifier? (token)
  (and (not (is-basic-math-operator? token))
       (not (is-number? token))
       (not (is-bool? token))
       (not (is-special-token? token))))

(defun make-identifier-node (token)
  (make-ast-node "identifier" token))

(defun is-basic-math-operator? (token)
  (member token '("+" "/" "*" "-") :test #'string=))

(defun is-number? (token)
  (not (null (parse-integer token :junk-allowed t))))

(defun is-bool? (token)
  (if (or (match token "true")
	  (match token "false"))
      t
      nil))

(defun make-value-node(type value)
  (let ((type-node (make-ast-node "type" type))
	(value-node (make-ast-node "value" value)))
    (make-ast-node "expression-value" (list type-node value-node))))

(defun make-expression-leaf-node (tokenizer expr-list)
  (with-parse-expression (tokenizer)
    (cond ((class-function-call? tokenizer)
	   (push-set (make-function-call tokenizer) expr-list))
          ((is-number? token)
	   (push-set (make-value-node "number" token) expr-list))
	  ((is-bool? token)
	   (push-set (make-value-node "bool" token) expr-list))
	  ((is-basic-math-operator? token)
	   (push-set (make-value-node "math-operator" token) expr-list))
	  ((is-nested? token)
	   (push-set
	    (make-ast-node "nested-expression"
			   (make-expression-leaf-node tokenizer ()))
	    expr-list))
	  ((match-para-end token) (return expr-list))
	  ((match-comma token) (return expr-list))
	  ((is-identifier? token)
	   (push-set (make-identifier-node token) expr-list))
	  (t (push-set (make-identifier-node "error parsing expression") expr-list))))
  expr-list)

(defun expression (tokenizer expr-list)
  (make-expression-leaf-node tokenizer expr-list))
  
(defun parse-expression (tokenizer)
  (let ((expr-list (expression tokenizer ())))
    (let ((expression-node expr-list))
      expression-node)))
