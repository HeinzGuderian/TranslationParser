(in-package :ast-node-space)

(defun match-shallow-ast-node (node symbol value &optional (test-fn #'equal))
  (let ((node-symbol (symbol-from-ast-node node))
	(node-data (data-from-ast-node node)))
    (and (same-node-symbol? symbol node-symbol)
	 (if (consp node-data)
	     (code-generator-utils-space:simple-list-identical? value node-data)
	     (funcall test-fn value node-data)))))
