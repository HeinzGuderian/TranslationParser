(in-package :ast-node-space)

(defun match-shallow-ast-node (node test-node &optional (test-fn #'equal))
  (let ((node-symbol (symbol-from-ast-node node))
	(node-data (data-from-ast-node node))
	(test-node-symbol (symbol-from-ast-node test-node))
	(test-node-data (data-from-ast-node test-node)))
    (and (same-node-symbol? test-node-symbol node-symbol)
	 (if (consp node-data)
	     (code-generator-utils-space:simple-list-identical? test-node-data node-data)
	     (funcall test-fn test-node-data node-data)))))

;; We assume that there are only two types of noded, edge and comples, no mixes
;; basic (SYMBOL "DATA" ...)
;; complex (SYMBOL (SYMBOL "DATA") ...)
(defun test-ast-tree (tokenized-tree test-tree)
  (labels ((any-but-not-equal (t1 t2)
	     (and (or t1 t2)
		  (not (equal t1 t2))))
	   (rec-test (walk-tok walk-test)
	     (if (any-but-not-equal (null walk-tok) (null walk-test))
		 nil
		 (if (and (null walk-tok) ;; end of tree?
			  (null walk-test))
		     t
		     (let* ((tok-node (access-walk-node walk-tok))
			    (test-node (access-walk-node walk-test))
			    (is-tok-node-edge-node (ast-node-edge-node? tok-node))
			    (is-test-node-edge-node (ast-node-edge-node? test-node)))
		       (if (any-but-not-equal is-tok-node-edge-node
					      is-test-node-edge-node) ;basic check that they are the same type of node
			   nil
			   (if (and is-tok-node-edge-node
				    is-test-node-edge-node)
			       (if (match-shallow-ast-node tok-node test-node)
				   (rec-test (next-walk-node walk-tok) (next-walk-node walk-test))
				   nil)
			       (rec-test (next-walk-node walk-tok) (next-walk-node walk-test)))))))))
    (rec-test (walk-ast-tree-dfs tokenized-tree) (walk-ast-tree-dfs test-tree))))
