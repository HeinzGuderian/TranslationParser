(asdf:defsystem "code-writer"
  :serial t
  :depends-on (#:cl-utilities)
  :components ((:file "package")
               (:file "tokenizer")
	       (:file "tokenizer-object")
	       (:file "tokenizer-test-system")
	       (:file "code-generator-utils")
	       (:file "ast-node")
	       (:file "ast-node-test")
	       (:file "csharp-parser")
	       (:file "csharp-parser-tests")))
