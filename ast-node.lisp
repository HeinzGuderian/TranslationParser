(in-package :ast-node-space)

(defun make-ast-node (symbol data)
  (cons (make-ast-symbol symbol) data))

(defun push-node (node tree)
      (push node (cdr (last tree))))

(defun symbol-from-ast-node (node)
  (car node))

(defun data-from-ast-node (node)
  (cdr node))

(defun same-node-symbol? (sym1 sym2)
  (eq sym1 sym2))

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
    ;;(print sym)
    ;;(intern symbol-string)
    (import sym 'ast-node-space)
    (export sym 'ast-node-space)
    sym))

(defun node-stack-has-symbol-node? (node-stack symbol)
  (and (consp node-stack) 
       (member symbol node-stack :test #'same-node-symbol? :key #'symbol-from-ast-node)))

(defun node-stack-has-visibility-node? (node-stack)
  (node-stack-has-symbol-node? node-stack (make-ast-symbol "visibility")))

(defun node-stack-has-type-node? (node-stack)
  (node-stack-has-symbol-node? node-stack (make-ast-symbol "type")))


(defun find-symbol-in-stack (node-stack symbol)
  (find symbol node-stack :test #'same-node-symbol? :key #'symbol-from-ast-node))

(defun get-visibility-node-from-node-stack (node-stack)
  (find-symbol-in-stack node-stack (make-ast-symbol "visibility")))

(defun get-type-node-from-node-stack (node-stack)
  (find-symbol-in-stack node-stack (make-ast-symbol "type")))


(export-all-symbols :ast-node-space)
