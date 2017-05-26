(ql:quickload "cl-utilities")

(in-package :csharp-parser)

(declaim (optimize (debug 3)))
;; (in-package :csharp-parser)
;; (defparameter test (parse-csharp (tokenize-csharp-code *code-test-class*)))
(defparameter *code-test* 
"public void Start(container as List){
     return \"ArmyEconomy\";} ")
(defparameter *code-test-class* 
" 
using UnityEngine;
using UnityEngine;

partial public class FactoryEconomy : BuildingEconomy, IGUI {}"
)

(defparameter *code-test-full* 
" 
using UnityEngine;
using UnityEngine;

partial public class FactoryEconomy : BuildingEconomy, IGUI {
      private List<int> _playerGameObjectList;
      public GameObject _playerGameObject;
      bool _haveConceded = false;
      TeamScript.PlayerNumberEnum _winningPlayer;
      private int a = 3;
      private int b = 3;
      var b = 2;
}
")

(defmacro with-token-and-peek (&body body) 
  `(let ((token (current-token tokenizer))
	 (peek (peek-token tokenizer)))
     ,@body))

(defmacro with-token (&body body) 
  `(let ((token (current-token tokenizer)))
     ,@body))

(defun tokenize-csharp-code (string-to-parse)
  (tokenizer:tokenize-with-symbols '("public"
				     "class"
				     "return"
				     "partial"
				     ";"
				     "("
				     ")"
				     "{"
				     "}"
				     ","
				     "\""
				     "/"
				     "\Return"
				     "\LineFeed"
				     "["
				     "]")
				   string-to-parse))
;; ".")))))
  
(defun print-tokens (tokenizer)
  (do ((token (current-token tokenizer) (advanze-token tokenizer))
       (x ()))
      ((funcall (lambda (x) (null (current-token x))) tokenizer) (nreverse x))
    (push token x)))

" Bnf for basic Boo parsing:
File = Using Class
Imports = Using & Using
Import = 'using' String stmtEnd 
stmtEnd = ';' 
blockStart = '{'
blockClose = '}'

Class = ClassDeclaration ClassBody 
ClassDeclaration = 'partial'? 'public'? 'class' String ClassInheritance? 
ClassInheritance = ':' String (, String)? 
VaribleDeclaration = Type String
Type = int | string | bool

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

(defun match-adv (tokenizer symbol)
  (match (advanze-token tokenizer) symbol))

(defun parse-csharp (tokenizer)
  (let ((ast-tree (list(make-ast-node :file "name"))))
    (parse-file tokenizer ast-tree)
;;    ))
   ast-tree))

(defun match-end (tokenizer) 
  (match-adv tokenizer ";"))

(defun parse-file (tokenizer ast-tree)
  (parse-usings tokenizer ast-tree)
  (parse-class tokenizer ast-tree))

(defun parse-usings (tokenizer ast-tree)
  (let ((ast-node (parse-using tokenizer)))
    (if (not(null ast-node))
	(progn
	  (push-node ast-node ast-tree)
	  (advanze-token tokenizer)
	  (match-end tokenizer)
	  (parse-usings tokenizer ast-tree))
	nil)))

(defun parse-using (tokenizer)
  (let ((token (current-token tokenizer)))
    (if (match token "using")
	(make-ast-node :using (advanze-token tokenizer))
	nil)))

(defun parse-class (tokenizer ast-tree)
  (parse-class-declaration tokenizer ast-tree))
  ;;(parse-class-body tokenizer ast-tree ()))
    
(defun block-start (tokenizer) 
    (match-adv tokenizer "{"))

(defun block-end (tokenizer) 
    (match-adv tokenizer "}"))

(defun parse-class-declaration (tokenizer ast-tree)
  (let* ((class-declaration-node (make-ast-node :class-declaration ()))
	 (class-modifiers-node (make-ast-node :class-visibility (grab-tokens-until tokenizer "class")))
	 (class-name-node (make-ast-node :class-name (advanze-token tokenizer))))
      (push-node class-modifiers-node class-declaration-node)
      (push-node class-name-node class-declaration-node)
      (push-node class-declaration-node ast-tree)
      ;;(advanze-token tokenizer)
      (when (not (block-start tokenizer))
	(progn
	  (advanze-token tokenizer)
	  (let ((class-inheritance-node (make-ast-node 
					 :class-inheritances 
					 (grab-tokens-until-filtered tokenizer
								     "{"
								     ","))))
	    (push-node class-inheritance-node class-declaration-node)))
	ast-tree)))

(defun parse-class-param-list (tokenizer)
  (let* ((param-list (grab-tokens-until tokenizer ")"))
	 (ast-node (make-ast-node :class-parameters (delete "," param-list :test #'string=))))
    ast-node))

(defun grab-tokens-until-filtered (tokenizer end-string seperator)
  (delete seperator 
	  (grab-tokens-until tokenizer end-string)
	  :test
	  #'string=))

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

(defun expression (tokenizer node-stack)
  (with-token-and-peek
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
  (with-token-and-peek (make-ast-node :identifier token)))

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
