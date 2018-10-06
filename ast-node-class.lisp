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

(defun list-nodes (node)
  (let ((node-listed (list (symbol-from-ast-node node)
			   (data-from-ast-node node))))
    (if (null (subnodes node))
	node-listed
	(append node-listed
		(mapcar (lambda (sub-node) (list-nodes sub-node))
			(subnodes node))))))

;; (let ((top-node (make-ast-node "top" 1))
;; 		      (child-node-1 (make-ast-node "child of top" 2))
;; 		      (child-node-21 (make-ast-node "child of child 1" 3))
;; 		      (child-node-12 (make-ast-node "child of top node" 4)))
;; 		  (push-node child-node-21 child-node-1)
;; 		  (push-node child-node-1 top-node)
;; 		  (push-node child-node-12 top-node)
;; 		  (print-subnodes-rec top-node)
;; 		  (list-nodes top-node))
