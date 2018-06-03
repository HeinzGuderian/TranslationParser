(in-package :ast-node-space)

(defclass ast-node-base-class ()
  ((parsed-node  :accessor parsed-node)))
(defgeneric is-node-type? (node tokenizer node-stack)
  (:documentation "Analyses the state and determines if it is a node of this type."))
(defgeneric make-node (node tokenzier node-stack))
