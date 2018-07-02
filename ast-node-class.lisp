(in-package :ast-node-space)

(defclass ast-node ()
  ((symbol
    :initarg :symbol
    :reader symbol-from-ast-node)
   (data
    :initarg :data
    :initform (list)
    :reader data-from-ast-node
    :accessor data)))

(defun make-ast-node (symbol data &optional (class 'ast-node))
  (make-instance class :symbol symbol :data (if (consp data) data (list data))))

(defun push-node (node tree)
  (if (null tree)
      node
      (push node (data (last tree)))))
