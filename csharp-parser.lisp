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

(defun match-end (tokenizer) 
  (match-cur tokenizer ";"))
    
(defun block-start (tokenizer) 
  (match-cur tokenizer "{"))

(defun block-end (tokenizer) 
  (match-cur tokenizer "}"))

(defun parse-csharp (tokenizer)
  (let ((ast-tree (list(make-ast-node "file" "name"))))
    (parse-file tokenizer ast-tree)
;;    ))
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
  (parse-class-body tokenizer ast-tree ()))

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

(defun strip-commas-from-string-list (param-list)
  (delete "," param-list :test #'string=))

(defun parse-class-param-list (tokenizer)
  (parse-param-list tokenizer "class-parameters"))

(defun parse-function-param-list (tokenizer)
  (parse-param-list tokenizer "function-parameter" #'code-generator-utils-space:make-pairs))

(defun parse-param-list (tokenizer ast-node-name &optional (param-transform-fn nil))
  (let ((param-list (grab-tokens-until tokenizer ")")))
    (if (null param-transform-fn)
        (make-ast-node ast-node-name (strip-commas-from-string-list param-list))
        (make-ast-node ast-node-name (funcall param-transform-fn (strip-commas-from-string-list param-list))))))

(defun parse-class-body (tokenizer ast-tree node-stack)
  (loop do 
       (progn (print (current-token tokenizer))
	      (print (peek-token tokenizer))
	      (cond ((class-fn? tokenizer node-stack)(make-class-function tokenizer node-stack)(push-node "function" ast-tree))
		    ((class-variable? tokenizer node-stack) 
		     (push-node (make-class-variable tokenizer node-stack) ast-tree)
		     (setq node-stack nil)
		     (advanze-token tokenizer))
		    ((eq (peek-token tokenizer) nil) (print "end finito"))
		    (t (progn
			 (if (consp node-stack)
			     (push-node (parse-token-to-ast-node tokenizer) node-stack)
			     (setf node-stack (list (parse-token-to-ast-node tokenizer)))))))
	      (advanze-token tokenizer))
     while (not(eq (peek-token tokenizer) nil))))

(defun make-class-function (tokenizer node-stack)
  (with-token-and-peek
      (let ((visibility-node (make-or-get-visibility-node node-stack))
	    (type-node (make-or-get-visibility-node node-stack))
	    (fn-name (current-token tokenizer)))
	(advanze-token tokenizer)
	(advanze-token tokenizer)
	(let ((param-pairs (parse-function-param-list tokenizer)))
	  (print param-pairs)))))

(defun class-fn? (tokenizer node-stack)
  (with-token-and-peek
    (and (node-stack-has-visibility-node? node-stack)
	 (node-stack-has-type-node? node-stack)
	 (match peek "("))))

(defun parse-token-to-ast-node (tokenizer)
  (cond ((visibility? (current-token tokenizer)) (make-visibility-node tokenizer))
	(t (make-type-node tokenizer))))

(defun make-type-node (tokenizer)
  (with-token-and-peek
    (if (match peek "[")
	(let ((type-node (make-ast-node "type" (concatenate 'string token "[]"))))
	       (advanze-token tokenizer)
	       (advanze-token tokenizer)
	       type-node)
	(make-ast-node "type" token))))

(defun visibility? (string)
  (cond ((match string "private") t)
	((match string "public") t)
	((match string "partial") t)
	((match string "final") t)
	(t nil)))

(defun make-visibility-node (tokenizer)
  (let ((vis-list (grab-tokens-until-fn tokenizer (lambda (x) (not (visibility? (peek-token x)))))))
    (make-ast-node "visibility"
		   (if (consp vis-list)
		       (list vis-list (current-token tokenizer))
		       (current-token tokenizer)))))

;;(defun make-type-node (tokenizer)
;;  (make-ast-node "type" (current-token tokenizer)))

(defun class-variable? (tokenizer node-stack)
  (with-token-and-peek
    (or (match peek "=") 
	(match peek ";"))))

(defun make-or-get-visibility-node(node-stack)
  (if (node-stack-has-visibility-node? node-stack)
      (get-visibility-node-from-node-stack node-stack)
      (make-ast-node "visibility" nil)))

(defun make-or-get-type-node(node-stack)
  (if (node-stack-has-type-node? node-stack)
      (get-type-node-from-node-stack node-stack)
      (make-ast-node "type" nil)))

(defun make-class-variable (tokenizer node-stack)
  (if (not (or (match (peek-token tokenizer) ";")
	       (match (peek-token tokenizer) "=")))
      (print "error parsing variable"))
  (let* ((visibility-node (make-or-get-visibility-node node-stack))
	 (type-node (make-or-get-type-node node-stack))
	 (name (current-token tokenizer))
	 (value (if (match (peek-token tokenizer) "=")
		    (progn
		      (advanze-token tokenizer)
		      (advanze-token tokenizer))
		    nil)))
    (make-ast-node "class-variable" 
		   (list visibility-node
			 (make-ast-node "variable-name" name)
			 type-node
			 (make-ast-node "value" value)))))

(defun expression (tokenizer node-stack)
  (with-token-and-peek
      (cond ((is-number? token) (make-value-node "number" token))
	    ((is-bool? token) (make-value-node "bool" token))
	    ((is-identifier? token peek) (make-identifier-node tokenizer node-stack))
	    (t (make-ast-node (make-ast-symbol "identifier") "bla")))))
	     

(defun is-identifier? (token peek)
  (if (or (match peek ".")
	  t)
      t
      t))

(defun make-identifier-node (tokenizer node-stack)
  (with-token-and-peek tokenizer (make-ast-node "identifier" token)))

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
	(t nil)))

(defun make-value-node(type value)
  (let ((type-node (make-ast-node "type" type)))
    (make-ast-node "value" (cons type-node value))))

