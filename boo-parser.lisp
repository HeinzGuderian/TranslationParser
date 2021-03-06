(ql:quickload "cl-utilities")

(defpackage :boo-parser
  (:use :common-lisp :tokenizer))
(in-package :boo-parser)

(declaim (optimize (debug 3)))
;; (in-package :boo-parser)
;; (defparameter test (parse-boo (tokenize-with-symbols *code-test-class*)))
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
      _winningPlayer as TeamScript.PlayerNumberEnum
      private a as int = 3
      private b = 3
      c = 4
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
							    ;; ".")))))
  
(defun print-tokens (tokenizer)
  (do ((token (current-token tokenizer) (advanze-token tokenizer))
       (x ()))
      ((funcall (lambda (x) (null (current-token x))) tokenizer) (nreverse x))
    (push token x)))

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
    (push-node class-modifiers-node ast-tree)
    (push-node class-name-node ast-tree)
    (push-node class-parameter-list ast-tree)
    (advanze-token tokenizer)
    (advanze-token tokenizer)
    ast-tree))

(defun parse-class-param-list (tokenizer)
  (let* ((param-list (grab-tokens-until tokenizer ")"))
	 (ast-node (make-ast-node :class-parameters (delete "," param-list :test #'string=))))
    ast-node))

(defun grab-tokens-until (tokenizer end-string)
  (grab-tokens-until-fn tokenizer (lambda (x) (string= (current-token x) end-string))))

(defun grab-tokens-until-fn (tokenizer end-fn)
  (do ((token (current-token tokenizer) (advanze-token tokenizer))
       (x ()))
      ((funcall end-fn tokenizer) (nreverse x))
    (push token x)))

(defun parse-class-body (tokenizer ast-tree node-stack)
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

(defun node-stack-has-visibility-node? (node-stack)
  (and (consp node-stack) 
       (same-node-symbol 
	(symbol-from-ast-node (car node-stack))
	:visibility)))

(defun get-visibility-node-from-node-stack (node-stack)
  (car node-stack))

(defun class-fn? (token node-stack)
  (and (node-stack-has-visibility-node? node-stack)
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
  (let ((vis-list (grab-tokens-until-fn tokenizer (lambda (x) (not (visibility? (current-token x)))))))
    (make-ast-node :visibility vis-list)))

(defun class-variable? (tokenizer node-stack)
  (let ((token (current-token tokenizer))
	(peek (peek-token tokenizer)))
    (or (match peek "as") 
	(match peek "="))))

(defun parse-class-variable (tokenizer ast-tree node-stack)
  (push-node (make-class-variable tokenizer node-stack) ast-tree)
  (setf node-stack nil)
  (advanze-token tokenizer))

(defun make-class-variable (tokenizer node-stack)
  (if (not (or (match (peek-token tokenizer) "as")
	       (match (peek-token tokenizer) "=")))
      "error parsing variable")
  (let* ((visibility (if (node-stack-has-visibility-node? node-stack)
			 (get-visibility-node-from-node-stack node-stack)
			 (make-ast-node :visibility nil)))
	 (name (current-token tokenizer))
	 (type (if (match (peek-token tokenizer) "as")
		   (progn
		     (advanze-token tokenizer)
		     (advanze-token tokenizer)
		     (current-token tokenizer))
		   nil))
	 (value (if (match (peek-token tokenizer) "=")
		    (progn
		      (advanze-token tokenizer)
		      (advanze-token tokenizer)
		      (current-token tokenizer));;node)
		    nil)))
    (make-ast-node :class-variable 
		   (list visibility
			 (make-ast-node :variable-name name)
			 (make-ast-node :type type)
			 (make-ast-node :value value)))))
  
(defmacro with-tokens (&body body) 
  `(let ((token (current-token tokenizer))
	 (peek (peek-token tokenizer)))
     ,@body))

(defun expression (tokenizer node-stack)
  (with-tokens
      (cond ((is-number? token) (make-value-node "number" token))
	    ((is-bool? token) (make-value-node "bool" token))
	    ((is-identifier? token peek) (make-identifier-node tokenizer node-stack))
	    (t (make-ast-node :identifier "bla")))))
	     

(defun is-identifier? (token peek)
  (if (or (match peek ".")
	  t)
      t
      t))

(defun make-identifier-node (tokenizer node-stack)
  (with-tokens (make-ast-node :identifier token)))

(defun is-bool? (token)
  (if (or (match token "true")
	  (match token "false"))
      t
      nil))

(defun is-number? (token)
  (not (null (parse-integer token :junk-allowed t))))

(defun make-value-node(type value)
  (let ((type-node (make-ast-node :type type)))
    (make-ast-node :value (cons type-node value))))
