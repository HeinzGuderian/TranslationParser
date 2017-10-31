(in-package :ast-node-space)

(defun make-ast-node (symbol data)
  (if (null data)
      (list (make-ast-symbol symbol))
      (if (consp data)
	  (cons (make-ast-symbol symbol) data)
	  (cons (make-ast-symbol symbol) (list data)))))

(defun push-node (node tree)
  (if (null tree)
      node
      (push node (cdr (last tree)))))

(defun symbol-from-ast-node (node)
  (car node))

(defun data-from-ast-node (node)
  (cdr node))

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
  (let ((sym (intern (string-upcase symbol-string))))
    (unintern sym)
    (unintern sym 'ast-node-space)
    (import sym 'ast-node-space)
    (export sym 'ast-node-space)
    sym))

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

  
