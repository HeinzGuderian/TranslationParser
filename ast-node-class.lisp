(in-package :ast-node-space)

(defclass ast-node-base-class ()
  ((symbol
    :initarg :symbol
    :reader symbol)
   (data :accessor data)))
(defgeneric is-node-type? (node tokenizer node-stack)
  (:documentation "Analyses the state and determines if it is a node of this type."))
(defgeneric make-node (node tokenzier node-stack))
