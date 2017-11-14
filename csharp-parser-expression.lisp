(ql:quickload "cl-utilities")
(ql:quickload "alexandria")
(in-package :csharp-parser)

(defmacro with-parse-expression ((tokenizer node-stack) &body body)
  `(loop do
	(progn (cond ,@body
		     (t (progn
			  (if (consp ,node-stack)
			      (push-node (make-expression-leaf-node ,tokenizer) ,node-stack)
			      (setf ,node-stack (list (make-expression-leaf-node ,tokenizer)))))))
	       (advanze-token ,tokenizer))
      while (and (not (eq (peek-token ,tokenizer) nil))
		 (not (match-end (peek-token ,tokenizer))))))

(defun is-identifier? (peek)
  (if (or (match peek ".")
	  t)
      t
      t))

(defun make-identifier-node (tokenizer)
  (with-token tokenizer (make-ast-node "identifier" token)))

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
  (let ((type-node (make-ast-node "type" type)))
    (make-ast-node "value" (cons type-node value))))

(defun expression (tokenizer node-stack ast-tree)
  (with-parse-expression (tokenizer node-stack) ))

(defun make-expression-leaf-node (tokenizer)
  (with-token-and-peek
    (cond ((is-number? token) (make-value-node "number" token))
	  ((is-bool? token) (make-value-node "bool" token))
	  ((is-identifier? peek) (make-identifier-node tokenizer))
	  (t (make-identifier-node tokenizer) "bla"))))

(defun parse-expression (tokenizer node-stack ast-tree) ;; fixa
  (advanze-token tokenizer)
  (let ((return-node (make-ast-node "function-return" (list (expression tokenizer node-stack)))))
    return-node))
