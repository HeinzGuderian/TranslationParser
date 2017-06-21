(asdf:defsystem "code-writer"
  :serial t
  :depends-on (#:cl-utilities)
  :components ((:file "package")
               (:file "tokenizer")
			   (:file "tokenizer-object")
			   (:file "code-generator-utils")
			   (:file "ast-node")
			   (:file "csharp-parser")
			   (:file "csharp-parser-tests")))