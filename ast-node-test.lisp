(in-package :ast-node-space)

(define-condition ast-node-mismatch-error (error)
  ((text :initarg :text :reader text)))

(defun match-shallow-ast-node (node test-node &optional (test-fn #'equal))
  (let ((node-symbol (symbol-from-ast-node node))
	(node-data (data-from-ast-node node))
	(test-node-symbol (symbol-from-ast-node test-node))
	(test-node-data (data-from-ast-node test-node)))
    (and (same-node-symbol? test-node-symbol node-symbol)
	 (if (consp node-data)
	     (code-generator-utils-space:simple-list-identical? test-node-data node-data)
	     (funcall test-fn test-node-data node-data)))))



(defun error-report (1exist 2exist 1node 2node &optional (error-message ""))
  (cond ((and 1exist 2exist) (error 'ast-node-mismatch-error :text
				    (concatenate 'string "Wrong symbol: '" (string (symbol-from-ast-node 1node))
						 "' Control symbol: '" (string (symbol-from-ast-node 2node))
						 "  " error-message)))
	(1exist (error 'ast-node-mismatch-error :text
				    (concatenate 'string "First symbol: '" (string (symbol-from-ast-node 1node))
						 "' Second symbol do not exist "
						 "  " error-message)))
	(2exist (error 'ast-node-mismatch-error :text
				    (concatenate 'string "Second symbol: '" (string (symbol-from-ast-node 2node))
						 "' First symbol do not exist "
						 "  " error-message)))
	(t (error 'ast-node-mismatch-error :text " Unknown error"))))
	


;; We assume that there are only two types of noded, edge and comples, no mixes
;; basic (SYMBOL "DATA" ...)
;; complex (SYMBOL (SYMBOL "DATA") ...)
(defun test-ast-tree (tokenized-tree test-tree)
  (labels ((any-but-not-equal (t1 t2)
	     (and (or t1 t2)
		  (not (equal t1 t2))))
	   (rec-test (walk-tok walk-test)
	     (if (any-but-not-equal (null walk-tok) (null walk-test))
		 (cond ((null walk-tok) (error 'ast-node-mismatch-error :text
					       (concatenate " org token no exist, control is: "
							   (symbol-from-ast-node (access-walk-node walk-tok)))))
		       ((null walk-test) (error 'ast-node-mismatch-error :text
						(concatenate " control token no exist, org is: "
							     (symbol-from-ast-node (access-walk-node walk-test))))))
		 (if (and (null walk-tok) ;; end of tree?
			  (null walk-test))
		     t
		     (let* ((tok-node (access-walk-node walk-tok))
			    (test-node (access-walk-node walk-test))
			    (is-tok-node-edge-node (ast-node-edge-node? tok-node))
			    (is-test-node-edge-node (ast-node-edge-node? test-node)))
		       (if (any-but-not-equal is-tok-node-edge-node
					      is-test-node-edge-node) ;basic check that they are the same type of node
			   (error-report t t tok-node test-node "Not the same type ")
			   (if (and is-tok-node-edge-node
				    is-test-node-edge-node)
			       (if (match-shallow-ast-node tok-node test-node)
				   (rec-test (next-walk-node walk-tok) (next-walk-node walk-test))
				   (error-report t t tok-node test-node "They do not match"))
			       (rec-test (next-walk-node walk-tok) (next-walk-node walk-test)))))))))
    (rec-test (walk-ast-tree-dfs tokenized-tree) (walk-ast-tree-dfs test-tree))))
