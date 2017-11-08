(ql:quickload "cl-utilities")
(in-package :csharp-parser)


(defun is-identifier? (peek)
  (if (or (match peek ".")
	  t)
      t
      t))

(defun make-identifier-node (tokenizer node-stack)
  (with-token tokenizer (make-ast-node "identifier" token)))

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

(defun expression (tokenizer node-stack)
  (with-token-and-peek
      (cond ((is-number? token) (make-value-node "number" token))
	    ((is-bool? token) (make-value-node "bool" token))
	    ((is-identifier? peek) (make-identifier-node tokenizer node-stack))
	    (t (make-identifier-node tokenizer node-stack) "bla"))))
	     
