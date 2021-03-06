(in-package :ast-node-space)

(defun same-node-symbol? (sym1 sym2)
  (eq sym1 sym2))

(defun ast-node-edge-node? (node)
  (not (every #'consp (data-from-ast-node node))))

(defun match (token symbol)
  (string= token symbol))

(defun match-adv (tokenizer symbol)
  (match (advanze-token tokenizer) symbol))

(defun match-cur (tokenizer symbol)
  (match (current-token tokenizer) symbol))

(defun make-ast-symbol (symbol-string)
  (let ((sym (find-symbol symbol-string)))
    (if (not (null sym))
	sym
	(let ((new-sym (intern (string-upcase symbol-string))))
	  (import new-sym 'ast-node-space)
	  (export new-sym 'ast-node-space)
	  new-sym))))

(defun node-stack-has-symbol-node? (node-stack symbol)
  (and (consp node-stack) 
       (member symbol node-stack :test #'same-node-symbol? :key #'symbol-from-ast-node)))

(defun find-symbol-in-stack (node-stack symbol)
  (find symbol node-stack :test #'same-node-symbol? :key #'symbol-from-ast-node))

(defun create-ast-walk-node (return-node continue-fn)
  (cons return-node continue-fn))

(defun access-walk-node (walk-node)
  (car walk-node))

(defun next-walk-node (walk-node)
  (funcall (cdr walk-node)))

(defun walk-collect-all-ast-nodes (ast-tree)
  (loop for x = (walk-ast-tree-dfs ast-tree) then (next-walk-node x) when (not(null(access-walk-node x))) collecting (access-walk-node x ) until (null (access-walk-node x))) )

(defun walk-ast-tree-dfs (tokenized-tree)
  (labels ((return-continue-fn (tree fn-list)
	     (lambda () (rec-test tree fn-list)))
	   (rec-test (tok-rec-tree continue-fns)
	     (if (null tok-rec-tree) ; end of tree?
		 (if (null continue-fns)
		     nil
		     (funcall continue-fns))
		 (let* ((tok-node (car tok-rec-tree))
			(is-tok-node-edge-node (ast-node-edge-node? tok-node)))
		   ;(print tok-node)
		   (if is-tok-node-edge-node
		       (cons tok-node (return-continue-fn (cdr tok-rec-tree)
							  continue-fns))
		       (cons tok-node (return-continue-fn (data-from-ast-node tok-node)
							  (return-continue-fn (cdr tok-rec-tree) continue-fns))))))))
    (rec-test tokenized-tree nil)))

  ;; Convert walk-ast-tree-dfs to trec ( On Lisp page 75 )
