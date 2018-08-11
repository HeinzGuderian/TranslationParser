(in-package :ast-node-space)

(defclass ast-node ()
  ((symbol
    :initarg :symbol
    :reader symbol-from-ast-node)
   (data
    :initarg :data
    :initform (list)
    :reader data-from-ast-node
    :accessor data)
   (subnodes
    :initarg :subnodes
    :initform (list)
    :accessor subnodes)))

(defun make-ast-node (symbol data &optional (class 'ast-node))
  (make-instance class
		 :symbol (make-ast-symbol symbol)
		 :data (if (consp data) data (list data))))

(defun push-node (node tree)
  (if (null tree)
      node
      (if (null (subnodes tree))
	  (setf (subnodes tree) (list node))
	  ;;(push node (cdr(last(subnodes tree)))))))
	  (setf (subnodes tree) (append (subnodes tree) (list node))))))

(defun print-node (node)
  (describe node))
  ;;(format nil " ~a subnodes: ~a" (data node) (subnodes node)))

(defun print-subnodes-rec (node)
  (if (null (subnodes node))
      (progn (print-node node)nil)
      (progn
	(print-node node)
	(dolist (subnode (subnodes node)) (print-subnodes-rec subnode)))))

(defun subnth (subnode-index node)
  (nth subnode-index(subnodes node)))
