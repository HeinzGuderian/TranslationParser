(ql:quickload "cl-utilities")
(in-package :csharp-parser)

(defun match-end (tokenizer) 
  (match-cur tokenizer ";"))
    
(defun block-start (tokenizer) 
  (match-cur tokenizer "{"))

(defun block-end (tokenizer) 
  (match-cur tokenizer "}"))

(defun strip-comma-from-string-list (string-list)
  (code-generator-utils-space:strip-string-from-string-list string-list ","))
