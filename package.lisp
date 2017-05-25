(defpackage :tokenizer
  (:use :common-lisp)
  (:export :tokenize-with-symbols 
	   :current-token 
	   :advanze-token 
	   :peek-token))

(defpackage :csharp-parser
  (:use :common-lisp :tokenizer))
