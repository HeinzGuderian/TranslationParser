(defpackage :code-generator-utils-space
  (:use :common-lisp)
  (:export :export-all-symbols))

(defpackage :tokenizer
  (:use :common-lisp :code-generator-utils-space)
  (:export :tokenize-with-symbols 
	   :current-token 
	   :advanze-token 
	   :peek-token
	   :with-token-and-peek
	   :with-token
	   :print-tokens))

(defpackage :ast-node-space
  (:use :code-generator-utils-space :common-lisp))

(defpackage :csharp-parser
  (:use :common-lisp :code-generator-utils-space :tokenizer))
