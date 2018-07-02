1.
How to start:
(ql:quickload :code-writer)
(in-package :csharp-parser)
(run-ast-test-suite-csharp)
(run-tokenizer-test-suite-csharp)
(parse-csharp(tokenize-csharp-code *code-test-expression-archimetic-nested-minus* ))