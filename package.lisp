(defpackage :tokenizer
  (:use :common-lisp)
  (:export :tokenize-with-symbols 
	   :current-token 
	   :advanze-token 
	   :peek-token
	   :with-token-and-peek
	   :with-token
	   :print-tokens))

(defpackage :csharp-parser
  (:use :common-lisp :tokenizer))
