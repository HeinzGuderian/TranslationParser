(ql:quickload "cl-utilities")

(defpackage :boo-parser
  (:use :common-lisp :tokenizer))
(in-package :boo-parser)

(declaim (optimize (speed 0) (space 0) (debug 3)))

;; (defparameter test (parse-boo (tokenize-with-symbols *code-test*)))
(defparameter *code-test* 
"public def Start(container as List):
     return \"ArmyEconomy\" ")
(defparameter *code-test-class* 
" 
import UnityEngine
import UnityEngine

partial public class FactoryEconomy (BuildingEconomy, IGUI):
")

(defparameter *code-test-full* 
" 
import UnityEngine
import UnityEngine

partial public class FactoryEconomy (BuildingEconomy, IGUI):

     public def Start(container as List):
          return \"ArmyEconomy\"
")

(defun current-token (tokenizer)
  (funcall tokenizer :current))

(defun advanze-token (tokenizer)
  (funcall tokenizer :advanze))

(defun peek-token (tokenizer)
  (funcall tokenizer :peek))

(defun tokenize-with-symbols (string-to-tokenize)
  (tokenizer:tokenizer-object 
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
							     "\LineFeed")))))
  

" Bnf for basic Boo parsing:
File = Imports Class
Imports = Import & Import
Import = 'import' string 

Class = ClassDeclaration ( ClassParameters ) : body
ClassDeclaration = partial? public? class string
ClassParameters = string | string ,
"

(defun make-ast-node (symbol data)
  (cons symbol data))

(defun push-node (node tree)
  (push node (cdr (last tree))))

(defun parse-boo (tokenizer)
  (let ((ast-tree (list(cons :file "name"))))
    (parse-file tokenizer ast-tree)
;;    ))
    ast-tree))

(defun parse-file (tokenizer ast-tree)
  (parse-imports tokenizer ast-tree)
  (parse-class tokenizer ast-tree))

(defun parse-imports (tokenizer ast-tree)
  (let ((token (current-token tokenizer)))
    (if (string= token "import")
	(let ((ast-node (parse-import tokenizer)))
	  (if (not(null ast-node))
	      (progn
		(push-node ast-node ast-tree)
		(advanze-token tokenizer)
		(parse-imports tokenizer ast-tree))
	      nil)))))

(defun parse-import (tokenizer)
  (let ((token (current-token tokenizer)))
    (if (string= token "import")
	(make-ast-node :import (advanze-token tokenizer))
	nil)))

(defun parse-class (tokenizer ast-tree)
  (parse-class-declaration tokenizer ast-tree)
  ast-tree)
    

(defun parse-class-declaration (tokenizer ast-tree)
  (let* ((class-modifiers-node (make-ast-node :class-visibility (grab-tokens-until tokenizer "class")))
	 (class-name-node (make-ast-node :class-name (advanze-token tokenizer)))
	 (class-parameter-list (progn (advanze-token tokenizer)
				      (advanze-token tokenizer)
				      (parse-class-param-list tokenizer))))
    ;;(push class-visibility-node (cdr (last ast-tree)))
    (push-node class-modifiers-node ast-tree)
    (push-node class-name-node ast-tree)
    (push-node class-parameter-list ast-tree)
    ast-tree))
    ;;(let* ((class-name-node (cons :class-name (cadr token-list)))
	;;   (class-paras-stream (parse-class-param-list (cdddr token-list) ast-tree)))
	   ;;(token-list (nthcdr (+(car class-paras-stream)2) token-list)))
;;      (push class-name-node (cdr (last ast-tree)))
  ;;    token-list)))

(defun parse-class-param-list (tokenizer)
  (let* ((param-list (grab-tokens-until tokenizer ")"))
	 (ast-node (make-ast-node :class-parameters (delete "," param-list :test #'string=))))
    ast-node))

(defun grab-tokens-until (tokenizer end-string)
  (do ((token (current-token tokenizer) (advanze-token tokenizer))
       (x ()))
      ((string= token end-string) (nreverse x))
    (push token x)))

(defun parse-function (tokenizer ast-tree))

(defun parse-funcion-visibility (tokenizer ast-tree))
