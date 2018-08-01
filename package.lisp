(defpackage :code-generator-utils-space
  (:use :common-lisp)
  (:export :export-all-symbols
	   :simple-list-identical?
	   :dotted-pair?
	   :make-pairs
	   :strip-string-from-string-list))

(defpackage :tokenizer
  (:use :common-lisp :code-generator-utils-space)
  (:export :tokenize-with-symbols 
	   :current-token 
	   :advanze-token 
	   :peek-token
	   :with-token-and-peek
	   :with-token
	   :with-peek
	   :print-tokens
	   :grab-tokens-until
	   :grab-tokens-until-fn
	   :grab-tokens-until-filtered
	   :match-token-list
	   :token-mismatch-error))

(defpackage :ast-node-space
  (:use :common-lisp :code-generator-utils-space :tokenizer)
  (:export :make-ast-node :push-node :match :symbol-from-ast-node
	   :data-from-ast-node :same-node-symbol? :ast-node-edge-node?
	   :match :match-adv :match-cur :make-ast-symbol
	   :node-stack-has-symbol-node? :find-symbol-in-stack
	   :create-ast-walk-node :access-walk-node :next-walk-node
	   :walk-collect-all-ast-nodes :walk-ast-tree-dfs
	   :match-shallow-ast-node :test-ast-tree :ast-node-mismatch-error
	   :ast-node-base-class :make-node :parsed-node :data :symbol :subnodes
	   :print-subnodes-rec :print-node))

(defpackage :csharp-parser
  (:use :common-lisp :code-generator-utils-space :tokenizer :ast-node-space)
  (:export :tokenize-csharp-code
		:parse-csharp))
