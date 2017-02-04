(ql:quickload "cl-utilities")

(defpackage :boo-parser
  (:use :common-lisp :tokenizer))
(in-package :boo-parser)

(declaim (optimize (speed 0) (space 0) (debug 3)))

;; (defparameter test (parse-boo (tokenize-with-symbols *code-test*)))
(defvar *code-test* 
"def Start():
     return \"ArmyEconomy\" ")
(defvar *code-test-class* 
" 
import UnityEngine

partial publicclass FactoryEconomy (BuildingEconomy, IGUI):
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
;;    ))
    ast-tree))

(defun parse-file (token-list ast-tree)
  (let ((token (car token-list)))
    (if (string= token "import")  
	(let ((class-token-stream (parse-imports token-list ast-tree)))
	  (parse-class class-token-stream ast-tree))
	"fff")))

(defun parse-imports (token-list ast-tree)
  (let ((ast-node (cons :import (cadr token-list)))
	(token-list (nthcdr 2 token-list)))
    (push ast-node (cdr ast-tree))
    (if (string= (caddr token-list) "import")
	(parse-imports token-list ast-tree)
        token-list)))

(defun parse-class (token-list ast-tree)
  (let* ((parameter-stream (parse-class-declaration token-list ast-tree)))
	 ;;(class-body-stream (parse-class-param-list token-list ast-tree)))
    ;;class-body-stream))
    parameter-stream))
    

(defun parse-class-declaration (token-list ast-tree)
  (let* ((class-modifiers (grab-tokens-until token-list "class"))
	 (class-visibility-node (cons :class-visibility (cadr class-modifiers)))
	 (token-list (nthcdr (car class-modifiers) token-list)))
    (push class-visibility-node (cdr (last ast-tree)))
    (let* ((class-name-node (cons :class-name (cadr token-list)))
	   (class-paras-stream (parse-class-param-list (cdddr token-list) ast-tree)))
	   ;;(token-list (nthcdr (+(car class-paras-stream)2) token-list)))
      (push class-name-node (cdr (last ast-tree)))
      token-list)))

(defun parse-class-param-list (token-list ast-tree)
  (let* ((param-list (grab-tokens-until token-list ")"))
	 (ast-node (cons :class-parameters (delete "," (cadr param-list) :test #'string=)))
	 (token-list (nthcdr (+ (car param-list)2) token-list)))
    (push ast-node (cdr (last ast-tree)))
    token-list))

(defun grab-tokens-until (token-list end-word)
  (loop for x in token-list
		 until (string= x end-word)
		 counting x into i
		 collecting x into y
		 finally (return (list i y))))

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
