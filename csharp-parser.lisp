(ql:quickload "cl-utilities")
(in-package :csharp-parser) 
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
(defun parse-csharp (tokenizer)
  (let ((ast-tree (list(make-ast-node "file" "name"))))
    (parse-file tokenizer ast-tree)
   ast-tree))

(defun parse-file (tokenizer ast-tree)
  (parse-usings tokenizer ast-tree)
  (parse-class tokenizer ast-tree))

(defun parse-usings (tokenizer ast-tree)
  (let ((ast-node (parse-using tokenizer)))
    (if (not(null ast-node))
	(progn
	  (push-node ast-node ast-tree)
	  (advanze-token tokenizer)
	  (advanze-token tokenizer)
	  (parse-usings tokenizer ast-tree))
	nil)))

(defun parse-using (tokenizer)
  (let ((token (current-token tokenizer)))
    (if (match token "using")
	(make-ast-node "using" (advanze-token tokenizer))
	nil)))

(defun parse-class (tokenizer ast-tree)
  (parse-class-declaration tokenizer ast-tree)
  (advanze-token tokenizer)
  (parse-class-body tokenizer () ast-tree))

(defun parse-class-declaration (tokenizer ast-tree)
  (let* ((class-declaration-node (make-ast-node "class-declaration" ()))
	 (class-modifiers-node (make-ast-node "class-visibility" (grab-tokens-until tokenizer "class")))
	 (class-name-node (make-ast-node "class-name" (advanze-token tokenizer))))
      (push-node class-modifiers-node class-declaration-node)
      (push-node class-name-node class-declaration-node)
      (push-node class-declaration-node ast-tree)
      (advanze-token tokenizer)
      (when (not (block-start tokenizer))
	(progn
	  (advanze-token tokenizer)
	  (let ((class-inheritance-node (make-ast-node 
					 "class-inheritances"
					 (grab-tokens-until-filtered tokenizer
								     "{"
								     ","))))
	    (push-node class-inheritance-node class-declaration-node)))
	ast-tree)))

(defun parse-class-param-list (tokenizer)
  (parse-param-list tokenizer "class-parameters"))

(defun parse-function-param-list (tokenizer)
  (parse-param-list tokenizer
		    "function-parameters"
		    #'(lambda (para-list)
			(mapcar #'(lambda (pairs)
				    (make-ast-node "function-parameter"
					  (list (make-ast-node "type" (car pairs))
					  (make-ast-node "variable-name" (cadr pairs)))))
				(code-generator-utils-space:make-pairs para-list)))))

(defun parse-class-body (tokenizer node-stack ast-tree)
  (with-parse-stmts (tokenizer node-stack)
    ((class-fn? tokenizer node-stack)
     (push-node (make-class-function tokenizer node-stack) ast-tree)
     (setq node-stack nil)
     (advanze-token tokenizer))
    ((variable? tokenizer node-stack) 
     (push-node (make-class-variable tokenizer node-stack) ast-tree)
     (setq node-stack nil)
     (advanze-token tokenizer))))

(defun make-class-function (tokenizer node-stack)
    (destructuring-bind (visibility-node type-node) (read-vis-type node-stack) 
      (let ((fn-name (current-token tokenizer)))
	(advanze-token tokenizer)
	(advanze-token tokenizer)
	(let* ((param-pairs (parse-function-param-list tokenizer))
	       (function-ast (list visibility-node
				   type-node
				   (make-ast-node "function-name" fn-name)
				   param-pairs)))
	  (advanze-token tokenizer)
	  (advanze-token tokenizer)
	  (setq node-stack nil)
	  (parse-function-body tokenizer node-stack function-ast)
	  (make-ast-node "function-node" function-ast)))))

(defun class-fn? (tokenizer node-stack)
  (with-peek
    (and (node-stack-has-visibility-node? node-stack)
	 (node-stack-has-type-node? node-stack)
	 (match peek "("))))

(defun make-class-variable (tokenizer node-stack)
  (make-variable tokenizer node-stack "class-variable"))

(defun make-expression-node (tokenizer node-stack)
  (advanze-token tokenizer)
  (let ((return-node (make-ast-node "function-return" (list (expression tokenizer node-stack)))))
    return-node))

(defun is-identifier? (peek)
  (if (or (match peek ".")
	  t)
      t
      t))

(defun expression (tokenizer node-stack)
  (with-token-and-peek
      (cond ((is-number? token) (make-value-node "number" token))
	    ((is-bool? token) (make-value-node "bool" token))
	    ((is-identifier? peek) (make-identifier-node tokenizer node-stack))
	    (t (make-identifier-node tokenizer node-stack) "bla"))))
	     

(defun make-identifier-node (tokenizer node-stack)
  (with-token tokenizer (make-ast-node "identifier" token)))

(defun is-bool? (token)
  (if (or (match token "true")
	  (match token "false"))
      t
      nil))

(defun is-number? (token)
  (not (null (parse-integer token :junk-allowed t))))

(defun is-type? (tokenizer)
  (with-token-and-peek
    (if (or (is-basic-type? token)
	    (string= peek "."))
	t
	nil)))

(defun is-basic-type? (token)
  (cond ((string= token "int") t)
	((string= token "bool") t)
	((string= token "double") t)
	((string= token "float") t)
	((string= token "string") t)
	((string= token "char") t)
	((string= token "var") t)
	(t nil)))

(defun make-value-node(type value)
  (let ((type-node (make-ast-node "type" type)))
    (make-ast-node "value" (cons type-node value))))

