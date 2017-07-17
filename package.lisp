(defpackage :code-generator-utils-space
  (:use :common-lisp)
  (:export :export-all-symbols
	   :simple-list-identical?))

(defpackage :tokenizer
  (:use :common-lisp :code-generator-utils-space)
  (:export :tokenize-with-symbols 
	   :current-token 
	   :advanze-token 
	   :peek-token
	   :with-token-and-peek
	   :with-token
	   :print-tokens
	   :grab-tokens-until
	   :grab-tokens-until-fn
	   :grab-tokens-until-filtered
	   :match-token-list))

(defpackage :ast-node-space
  (:use :common-lisp :code-generator-utils-space :tokenizer)
  )
  ;;(:export :make-ast-node :push-node :match
;;	   :symbol-from-ast-node :data-from-ast-node))

(defpackage :csharp-parser
  (:use :common-lisp :code-generator-utils-space :tokenizer	:ast-node-space)
  (:export :tokenize-csharp-code
		:parse-csharp))
