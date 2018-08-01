(ql:quickload "cl-utilities")
(in-package :csharp-parser)

(defmacro define-match-fn (fn-name-supplied symbol-string)
  (let* ((package (symbol-package fn-name-supplied))
	 (fn-name (intern (format nil "MATCH-~a" fn-name-supplied)package))
	 (fn-token (intern "token" package)))
    `(defun ,fn-name (,fn-token) (match ,fn-token ,symbol-string))))

(define-match-fn end ";")
(define-match-fn block-start "{")
(define-match-fn block-end "}")
(define-match-fn assign "=")
(define-match-fn para-begin "(")
(define-match-fn para-end ")")
(define-match-fn comma ",")

(defun is-special-token? (token)
  (or (match-end token)
      (match-block-start token)
      (match-block-end token)
      (match-assign token)
      (match-para-begin token)
      (match-para-end token)))

(defun strip-commas-from-string-list (string-list)
  (code-generator-utils-space:strip-string-from-string-list string-list ","))

(defun parse-param-list (tokenizer ast-node-name &optional (param-transform-fn nil))
  (let ((param-list (grab-tokens-until tokenizer ")")))
    (if (null param-transform-fn)
        (make-ast-node ast-node-name (strip-commas-from-string-list param-list))
        (make-ast-node ast-node-name (funcall param-transform-fn (strip-commas-from-string-list param-list))))))

(defun make-type-node (tokenizer)
  (with-token-and-peek
     (if (match peek "[")
	(let ((type-node (make-ast-node "type" (concatenate 'string token "[]"))))
	       (advanze-token tokenizer)
	       (advanze-token tokenizer)
	       type-node)
	(make-ast-node "type" token))))

(defun visibility? (string)
  (cond ((match string "private") t)
	((match string "public") t)
	((match string "partial") t)
	((match string "final") t)
	(t nil)))

(defun make-visibility-node (tokenizer)
  (let ((vis-list (grab-tokens-until-fn tokenizer (lambda (x) (not (visibility? (peek-token x)))))))
    (make-ast-node "visibility"
		   (if (consp vis-list)
		       (list vis-list (current-token tokenizer))
		       (current-token tokenizer)))))

(defun parse-token-to-ast-node (tokenizer)
  (cond ((visibility? (current-token tokenizer)) (make-visibility-node tokenizer))
	(t (make-type-node tokenizer))))

(defmacro push-set (value target)
  `(if (consp ,target)
       (push-node ,value ,target)
       (setf ,target (list ,value))))

(defmacro with-parse-stmts ((tokenizer node-stack) &body body)
  `(loop do
	(progn (cond ,@body
		     (t (progn
			  (if (consp ,node-stack)
			      (push (parse-token-to-ast-node ,tokenizer) ,node-stack)
			      (setf ,node-stack (list (parse-token-to-ast-node ,tokenizer)))))))
	       (advanze-token ,tokenizer))
      while (and (not (eq (peek-token ,tokenizer) nil))
		 (not (match-block-end (peek-token ,tokenizer))))))


(defun make-visibility-symbol ()
  (make-ast-symbol "visibility"))

(defun make-type-symbol ()
  (make-ast-symbol "type"))

(defun node-stack-has-visibility-node? (node-stack)
  (node-stack-has-symbol-node? node-stack (make-visibility-symbol)))

(defun node-stack-has-type-node? (node-stack)
  (node-stack-has-symbol-node? node-stack (make-type-symbol)))

(defun get-visibility-node-from-node-stack (node-stack)
  (find-symbol-in-stack node-stack (make-visibility-symbol)))

(defun get-type-node-from-node-stack (node-stack)
  (find-symbol-in-stack node-stack (make-type-symbol)))

(defun make-or-get-visibility-node(node-stack)
  (if (node-stack-has-visibility-node? node-stack)
      (get-visibility-node-from-node-stack node-stack)
      (make-ast-node "visibility" nil)))

(defun make-or-get-type-node(node-stack)
  (if (node-stack-has-type-node? node-stack)
      (get-type-node-from-node-stack node-stack)
      (make-ast-node "type" nil)))

(defun read-vis-type (node-stack)
  (let* ((visibility-node (make-or-get-visibility-node node-stack))
	 (type-node (make-or-get-type-node node-stack)))
    (list visibility-node type-node)))

