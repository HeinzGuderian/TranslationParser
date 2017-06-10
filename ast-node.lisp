(in-package :ast-node-space)

(defun make-ast-node (symbol data)
  (cons symbol data))

(defun push-node (node tree)
      (push node (cdr (last tree))))

(defun symbol-from-ast-node (node)
  (car node))

(defun data-from-ast-node (node)
  (cdr node))

(defun same-node-symbol (sym1 sym2)
  (eq sym1 sym2))

(defun match (token symbol)
  (string= token symbol))

(defun match-adv (tokenizer symbol)
  (match (advanze-token tokenizer) symbol))

(defun match-cur (tokenizer symbol)
  (match (current-token tokenizer) symbol))

(defun node-stack-has-symbol-node? (tree symbol accessor-fn)
  (and (consp tree) 
       (same-node-symbol 
	(symbol-from-ast-node (funcall accessor-fn tree))
	symbol)))

(defun node-stack-has-visibility-node? (tree)
  (node-stack-has-symbol-node? tree :visibility #'car))

(defun get-visibility-node-from-node-stack (node-stack)
  (car node-stack))

(defun node-stack-has-type-node? (tree)
  (node-stack-has-symbol-node? tree :type #'cadr))

(defun get-type-node-from-node-stack (tree)
  (cadr tree))

(export-all-symbols :ast-node-space)
