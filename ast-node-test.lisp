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
	   (edge-node? (node-test)
	     (not (every #'consp node-test)))
	   (rec-test (tok-rec-tree test-rec-tree)
	     (if (or (null tok-rec-tree) ;; end of tree?
		     (null test-rec-tree))
		 t
		 (let* ((tok-node (car tok-rec-tree))
			(test-node (car test-rec-tree))
			(is-tok-node-edge-node (edge-node? tok-node))
			(is-test-node-edge-node (edge-node? test-node)))
		   (print tok-node)
		   (print test-node)
		   (if (any-but-not-equal is-tok-node-edge-node
					  is-test-node-edge-node) ;;basic chek that they are the same type of node
		       nil
		       (if (or (and (and is-tok-node-edge-node
					 is-test-node-edge-node)
				    (and (match-shallow-ast-node tok-node test-node)))
			       (and (not(and is-tok-node-edge-node
					     is-test-node-edge-node))
				    (every (lambda (x) (not (null x)))
					   (mapcar (lambda (x y) (rec-test x y)) (cdar tok-rec-tree) (cdar test-rec-tree)))))
			   (rec-test (cdr tok-rec-tree) (cdr test-rec-tree))
			   nil))))))
    (rec-test tokenized-tree test-tree)))

(export-all-symbols :ast-node-space)
