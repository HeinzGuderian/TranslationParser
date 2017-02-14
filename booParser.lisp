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
      private _playerGameObjectList as List
      public _playerGameObject as GameObject
      _haveConceded as bool = false
")

"
_currentPlayerGUI as GuiScript
      
      public LevelIsLoaded = false
      private final executeTurnString as string = \"executeTurn\"
 public def Start(container as List):
          return \"ArmyEconomy\" "
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
Import = 'import' String 

Class = ClassDeclaration ( ClassParameters ) : ClassBody
ClassDeclaration = partial? public? class String
ClassParameters = String | String , ClassParameters

ClassBody = ClassVariable | Field | Function
ClassVariable  = Visibility? String as String
Visibility = public final | public static final | public static | public | private | private final | 
private static final

Field = Visibility String as Type : FieldBody

"

(defun make-ast-node (symbol data)
  (cons symbol data))

(defun push-node (node tree)
  (push node (cdr (last tree))))

(defun symbol-from-ast-node (node)
  (car node))

(defun data-from-ast-node (node)
  (cdr node))

(defun same-node-symbol (sym1 sym2)
  (eq sym1 sym2))

(defun match (token symbol)
  (string= token symbol))

(defun parse-boo (tokenizer)
  (let ((ast-tree (list(make-ast-node :file "name"))))
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
  (parse-class-body tokenizer ast-tree ()))
    

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
    (advanze-token tokenizer)
    (advanze-token tokenizer)
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
  (grab-tokens-until-fn tokenizer (lambda (x) (string= x end-string))))

(defun grab-tokens-until-fn (tokenizer end-fn)
  (do ((token (current-token tokenizer) (advanze-token tokenizer))
       (x ()))
      ((funcall end-fn token) (nreverse x))
    (push token x)))

(defun parse-class-body (tokenizer ast-tree node-stack)
  ;;(if (null node-stack)
  ;;    (setf node-stack (list(parse-token-to-ast-node tokenizer))))
  (loop do 
       (let ((token (current-token tokenizer)))
	 (cond ((class-fn? token node-stack)
		"fn")
	       ((class-variable? tokenizer node-stack) 
		(push-node (make-class-variable tokenizer node-stack) ast-tree)
		(setq node-stack nil)
		(advanze-token tokenizer))
	       ((eq (peek-token tokenizer) nil) "end finito")
	       (t (progn
		    (if (eq nil node-stack)
			(setf node-stack (list(parse-token-to-ast-node tokenizer)))
			(push (parse-token-to-ast-node tokenizer) (cdr (last node-stack)))) ))))
       while (not(eq (peek-token tokenizer) nil))))
	       ;;(parse-class-body tokenizer ast-tree node-stack))))))

(defun class-fn? (token node-stack)
  (and (consp node-stack) 
		     (same-node-symbol 
		      (symbol-from-ast-node (car node-stack))
		      :visibility)
		     (match token "def")))

(defun parse-token-to-ast-node (tokenizer)
  (cond ((visibility? (current-token tokenizer)) (make-visibility-node tokenizer))
	(t (current-token tokenizer))))

(defun visibility? (string)
  (cond ((match string "private") t)
	((match string "public") t)
	((match string "partial") t)
	((match string "final") t)
	(t nil)))

(defun make-visibility-node (tokenizer)
  (let ((vis-list (grab-tokens-until-fn tokenizer (lambda (x) (not (visibility? x))))))
    (make-ast-node :visibility vis-list)))

(defun class-variable? (tokenizer node-stack)
  (let ((token (current-token tokenizer))
	(peek (peek-token tokenizer)))
    (or (and (not(consp node-stack))
	     (match peek "as")) 
	(and (consp node-stack) 
	     (same-node-symbol (symbol-from-ast-node (car node-stack))
			       :visibility))) ))

(defun parse-class-variable (tokenizer ast-tree node-stack)
  (push-node (make-class-variable tokenizer node-stack) ast-tree)
  (setf node-stack nil)
  (advanze-token tokenizer))

(defun make-class-variable (tokenizer node-stack)
  (if (not (match (peek-token tokenizer) "as"))
      "error parsing variable")
  (let* ((name (current-token tokenizer))
	 (type (progn
		 (advanze-token tokenizer)
		 (advanze-token tokenizer)))
	 (peek (peek-token tokenizer)))
    (let ((node (make-ast-node :class-variable 
		 (list (car node-stack)
		       (make-ast-node :variable-name name)
		       (make-ast-node :type type)))))
      (if (match peek "=")
	  (progn 
	    (push-node (make-ast-node :value (advanze-token tokenizer)) node)
	    node)
	  node))))

