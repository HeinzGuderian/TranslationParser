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

(defun test-ast-tree (tokenized-tree test-tree)
  (labels ((edge-node? (node-test is-dotted)
	     (or is-dotted ;; blir fel om cons och cdr är en lista
		 (some #'consp node-test)))
	   (rec-test (tok-rec-tree test-rec-tree)
	     (if (or (null tok-rec-tree)
		     (null test-rec-tree))
		 t
	     (let* ((tok-node (car tok-rec-tree))
		    (test-node (car test-rec-tree))
		    (is-tok-dotted (dotted-pair? tok-node))
		    (is-test-dotted (dotted-pair? test-node)))
	       (print tok-node)
	       (print test-node)
	       (if (or (null tok-node)
		       (null test-node))
		   t
	       (if (and (or is-tok-dotted
			    is-test-dotted)
			(not(equal is-tok-dotted
				   is-test-dotted)))
		   nil
		   (if (and (not (and is-tok-dotted
				      is-test-dotted))
			    (not(equal (list-length tok-node)
				       (list-length test-node))))
		       nil
	       (let ((is-tok-node-edge-node (edge-node? tok-node is-tok-dotted))
		     (is-rec-node-edge-node (edge-node? test-node is-test-dotted)))
		 (if (not (equal is-tok-node-edge-node
				 is-rec-node-edge-node))
		     nil
		 (rec-test (cdr tok-rec-tree) (cdr test-rec-tree)))))))))))
    (rec-test tokenized-tree test-tree)))

(export-all-symbols :ast-node-space)
