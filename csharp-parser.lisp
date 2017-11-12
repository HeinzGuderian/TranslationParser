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

" Bnf for basic C# parsing:
File = Using Class
Imports = Using & Using
Import = 'using' String stmtEnd 
stmtEnd = ';' 
blockStart = '{'
blockClose = '}'

Class = ClassDeclaration ClassBody 
ClassDeclaration = 'partial'? 'public'? 'class' String ClassInheritance? 
ClassInheritance = ':' String (, String)? 
VaribleDeclaration = String String

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
      (with-token
	(when (not (match-block-start token))
	  (progn
	    (advanze-token tokenizer)
	    (let ((class-inheritance-node (make-ast-node 
					   "class-inheritances"
					   (grab-tokens-until-filtered tokenizer
								       "{"
								       ","))))
	      (push-node class-inheritance-node class-declaration-node)))
	  ast-tree))))

(defun parse-class-param-list (tokenizer)
  (parse-param-list tokenizer "class-parameters"))

(defun make-class-variable (tokenizer node-stack)
  (make-variable tokenizer node-stack "class-variable"))

(defun parse-class-body (tokenizer node-stack ast-tree)
  (with-parse-stmts (tokenizer node-stack)
    ((class-fn? tokenizer node-stack)
     (push-node (make-class-function tokenizer node-stack) ast-tree)
     (setq node-stack nil)
     (advanze-token tokenizer))
    ((variable? tokenizer) 
     (push-node (make-class-variable tokenizer node-stack) ast-tree)
     (setq node-stack nil)
     (advanze-token tokenizer))))




