(ql:quickload "cl-utilities")

(defpackage :boo-parser
  (:use :common-lisp :tokenizer))
(in-package :boo-parser)

(declaim (optimize (speed 0) (space 0) (debug 3)))

;; (defparameter test (parse-c-sharp-fn-basic (tokenize-with-symbols *code-test*)))
(defvar *code-test* 
"def Start():
     return \"ArmyEconomy\" ")
(defvar *code-test-class* 
" 
import UnityEngine

class FactoryEconomy (BuildingEconomy, IGUI):
")


(defun tokenize-with-symbols (string-to-tokenize)
  (tokenizer:tokenize-fn string-to-tokenize
   (tokenizer:add-length-to-strings '("public"
     "def"
     "return"
     ";"
     "("
     ")"
     "{"
     "}"
     ","
     "\""
     "/"
     "\Return"
     "\LineFeed")))) 


" Bnf for basic Boo parsing:
File = Imports Class
Imports = Import & Import
Import = 'import' string 

Class = ClassDeclaration ( ClassParameters ) : body
ClassDeclaration = partial? public? class string
ClassParameters = string | string ,
"

(defun parse-boo (token-list)
  (let ((ast-tree (list(cons :file "name"))))
    (parse-file token-list ast-tree)
    ast-tree))

(defun parse-file (token-list ast-tree)
  (let ((token (car token-list)))
    (if (string= token "import")  
	(parse-imports token-list ast-tree)
	"fff")))

(defun parse-imports (token-list ast-tree)
  (let ((ast-node (cons :import (cadr token-list))))
    (push ast-node (cdr ast-tree))
    (if (string= (caddr token-list) "import")
	(parse-class(parse-imports (cddr token-list) ast-tree) ast-tree)
        ast-tree)))

(defun parse-class (token-list ast-tree)
  token-list)

(defun build-param-list (token-string-list new-list)
  (if (string-equal ")" (car token-string-list))
      (cons (nreverse new-list )
	    (parse-c-sharp-fn-basic-body (cdr token-string-list))) 
      (let ((current-token (cadr token-string-list))
	    (next-tokens (cddr token-string-list)))
	(build-param-list (nthcdr 3 token-string-list) 
			    (push (list current-token
					(car next-tokens))
				  new-list )))))

(defun parse-c-sharp-fn-basic-paras (token-string-list)
  (build-param-list token-string-list
		    (list )))

(defun parse-c-sharp-fn-basic-body (token-string-list) 
  (when (not(string-equal (car token-string-list) "{")) (error "body does not begin with {"))
  (list 
   (list-stmts (cdr token-string-list))))

(defun list-stmts (token-string-list)
  (let ((stmts-list ())
	(stmt ()))
    (dolist (token token-string-list) 
      (if (string-equal token ";")
	  (progn 
	    (push (nreverse stmt) stmts-list)
	    (setf stmt ()))
	  (push token stmt)))
  (nreverse stmts-list)))
