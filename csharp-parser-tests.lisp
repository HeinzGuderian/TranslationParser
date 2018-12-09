(in-package :csharp-parser) 

(declaim (optimize (debug 3)))

;; (in-package :csharp-parser)
;; (defparameter test (parse-csharp (tokenize-csharp-code *code-test-class*)))
;; command for generating test matches (print-tokens (tokenize-csharp-code *code-test-variables-advanced* ))
;; (print-tokens (tokenize-csharp-code *code-test-variables-simple* ))
;; (parse-csharp (tokenize-csharp-code *code-test-variables-simple*))

;;(match-shallow-ast-node (cadr(cadddr(parse-csharp(tokenize-csharp-code *code-test-variables-advanced*)))) '(class-visibility "partial" "public"))
;;(let ((parsed (parse-csharp(tokenize-csharp-code *code-test-variables-simple* ))))
;;(funcall (exist-node-in-tree #'*code-test-variables-simple-ast-tree*) (parse-csharp (tokenize-csharp-code *code-test-variables-simple*)))

(defparameter *code-test* 
"public void Start(container as List){
     return \"ArmyEconomy\";} ")


(defparameter *code-test-class-tokens* (list "using" "UnityEngine" ";" "using" "UnityEngine" ";" "partial" "public" "class" "FactoryEconomy" ":" "BuildingEconomy" "," "IGUI" "{" "}"))
(defparameter *code-test-class-ast-tree* '((FILE "name") (USING "UnityEngine") (USING "UnityEngine") (CLASS-DECLARATION (CLASS-VISIBILITY "partial" "public") (CLASS-NAME "FactoryEconomy") (CLASS-INHERITANCES "BuildingEconomy" "IGUI"))))
(defparameter *code-test-class* 
" 
using UnityEngine;
using UnityEngine;

partial public class FactoryEconomy : BuildingEconomy, IGUI {}"
)

(defparameter *code-test-variables-simple-tokens* (list "using" "UnityEngine" ";" "using" "UnityEngine" ";" "partial" "public" "class" "FactoryEconomy" ":" "BuildingEconomy" "," "IGUI" "{" "int" "c" ";" "private" "int" "b" "=" "2" ";" "private" "int" "a" ";" "}" ))


;;(test-composer (((is-var-sym? "variable-name") (is-class-sym? "class-variable"))
;;;		((test same-node-symbol? ())))
(defmacro with-is-symbol (binds &body body)
  (let ((symbols (mapcar #'(lambda (pair) (gensym)) binds)))
    `(let* (,@(mapcar #'(lambda (pair symbol)
			 `( ,symbol (make-ast-symbol ,(cadr pair))))
		     binds
		     symbols)
       ,@(mapcar #'(lambda (pair sym)
		     `( ,(car pair) (lambda (node)
				      (same-node-symbol? (symbol-from-ast-node node) ,sym))))
		 binds
		 symbols))
       ,@body)))

(defun *code-test-variables-simple-ast-tree* (parsed-tree)
  (let ((variable-sym (make-ast-symbol "variable-name"))
	(class-variable-sym (make-ast-symbol "class-variable")))
    ((lambda (node)
       (print-node node)
       (and (same-node-symbol? (symbol-from-ast-node node) class-variable-sym)
		(some #'(lambda (subnode) (and (same-node-symbol? (symbol-from-ast-node subnode)
								 variable-sym)
					       (equal "a" (car (data-from-ast-node subnode)))))
		      (subnodes node))))
     parsed-tree)))

  

(defun *code-test-variables-simple-ast-tree2* (node)
  (with-is-symbol ((variable-sym?  "variable-name")
		   (class-variable-sym?  "class-variable"))
    (let ((test1 #'(lambda (node)
		     (and (funcall class-variable-sym? node)
			  (some #'(lambda (subnode) (and (funcall variable-sym? subnode)
							 (equal "a" (car (data-from-ast-node subnode)))))
				(subnodes node))))))
      ((lambda (node)(and (funcall test1 node)))
       node))))

(defun test-node (node-identifier-fn node-test)
  (lambda (node)
    (and (funcall node-identifier-fn node)
	 (funcall node-test node))))
(defun test-some-subnode (test)
  (lambda (node) (some test (subnodes node))))
(defun test-car-data (test-data)
  (lambda (node) (equal test-data (car (data-from-ast-node node)))))

(defun *code-test-variables-simple-ast-tree3* (node)
  (with-is-symbol ((variable-sym?  "variable-name")
		   (class-variable-sym?  "class-variable"))
    (let ((test1 (test-node class-variable-sym? (test-some-subnode (test-node variable-sym? (test-car-data "a"))))))
      ((lambda (node) (funcall test1 node))
       node))))
 
(defparameter *code-test-variables-simple* 
" 
using UnityEngine;
using UnityEngine;

partial public class FactoryEconomy : BuildingEconomy, IGUI 
{
 int c;
 private int b = 2;
 private int a;
}"
  )

(defparameter *code-test-variables-custom-type-tokens*  (list "using" "UnityEngine" ";" "using" "UnityEngine" ";" "partial" "public" "class" "FactoryEconomy" ":" "BuildingEconomy" "," "IGUI" "{" "public" "GameObject" "_playerGameObject" ";" "TeamScript.PlayerNumberEnum" "_winningPlayer" ";" "private" "int" "a" "=" "2" ";" "}"))

(defparameter *code-test-variables-custom-type-ast-tree* '((FILE "name") (USING "UnityEngine") (USING "UnityEngine") (CLASS-DECLARATION (CLASS-VISIBILITY "partial" "public") (CLASS-NAME "FactoryEconomy") (CLASS-INHERITANCES "BuildingEconomy" "IGUI")) (CLASS-VARIABLE (VISIBILITY "public") (VARIABLE-NAME "_playerGameObject") (TYPE "GameObject") (VARIABLE-VALUE)) (CLASS-VARIABLE (VISIBILITY) (VARIABLE-NAME "_winningPlayer") (TYPE "TeamScript.PlayerNumberEnum") (VARIABLE-VALUE)) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "a") (TYPE "int") (VARIABLE-VALUE (EXPRESSION-VALUE (TYPE "number") (VALUE "2"))))) )

(defparameter *code-test-variables-custom-type* 
" 
using UnityEngine;
using UnityEngine;

partial public class FactoryEconomy : BuildingEconomy, IGUI {
      public GameObject _playerGameObject;
      TeamScript.PlayerNumberEnum _winningPlayer;
      private int a = 2;
}
")

(defparameter *code-test-variables-arrays-tokens*  (list "using" "UnityEngine" ";" "using" "UnityEngine" ";" "partial" "public" "class" "FactoryEconomy" ":" "BuildingEconomy" "," "IGUI" "{" "private" "int" "[" "]" "d" ";" "private" "int" "a" "=" "2" ";" "}"))

(defparameter *code-test-variables-arrays-ast-tree* '((FILE "name") (USING "UnityEngine") (USING "UnityEngine") (CLASS-DECLARATION (CLASS-VISIBILITY "partial" "public") (CLASS-NAME "FactoryEconomy") (CLASS-INHERITANCES "BuildingEconomy" "IGUI")) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "d") (TYPE "int[]") (VARIABLE-VALUE)) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "a") (TYPE "int") (VARIABLE-VALUE (EXPRESSION-VALUE (TYPE "number") (VALUE "2"))))) )

(defparameter *code-test-variables-arrays* 
" 
using UnityEngine;
using UnityEngine;

partial public class FactoryEconomy : BuildingEconomy, IGUI {
      private int[] d;
      private int a = 2;
}
")

(defparameter *code-test-class-function-tokens* (list "using" "UnityEngine" ";" "using" "UnityEngine" ";" "partial" "public" "class" "FactoryEconomy" ":" "BuildingEconomy" "," "IGUI" "{" "private" "int" "a" "=" "2" ";" "public" "int" "add" "(" "int" "b" "," "int" "c" ")" "{" "var" "d" "=" "3" ";" "return" "b" ";" "}" "private" "int" "e" "=" "5" ";" "}" ))

(defparameter *code-test-class-function-ast-tree* '((FILE "name") (USING "UnityEngine") (USING "UnityEngine") (CLASS-DECLARATION (CLASS-VISIBILITY "partial" "public") (CLASS-NAME "FactoryEconomy") (CLASS-INHERITANCES "BuildingEconomy" "IGUI")) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "a") (TYPE "int") (VARIABLE-VALUE (EXPRESSION-VALUE (TYPE "number") (VALUE "2")))) (FUNCTION-NODE (VISIBILITY "public") (TYPE "int") (FUNCTION-NAME "add") (FUNCTION-PARAMETERS (FUNCTION-PARAMETER (TYPE "int") (VARIABLE-NAME "b")) (FUNCTION-PARAMETER (TYPE "int") (VARIABLE-NAME "c"))) (FUNCTION-VARIABLE (VISIBILITY) (VARIABLE-NAME "d") (TYPE "var") (VARIABLE-VALUE (EXPRESSION-VALUE (TYPE "number") (VALUE "3")))) (FUNCTION-RETURN (IDENTIFIER "b"))) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "e") (TYPE "int") (VARIABLE-VALUE (EXPRESSION-VALUE (TYPE "number") (VALUE "5"))))))

(defparameter *code-test-class-function*
" 
using UnityEngine;
using UnityEngine;

partial public class FactoryEconomy : BuildingEconomy, IGUI {
      private int a = 2;
      public int add(int b, int c){
          var d = 3;
          return b;
      }
      private int e = 5;
}
")

(defparameter *code-test-expression-archimetic-simple-tokens* (list "using" "UnityEngine" ";" "public" "class" "FactoryEconomy" "{" "private" "int" "a" "=" "2" "+" "3" ";" "}"))
(defparameter *code-test-expression-archimetic-simple-ast-tree* '((FILE "name") (USING "UnityEngine") (CLASS-DECLARATION (CLASS-VISIBILITY "public") (CLASS-NAME "FactoryEconomy")) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "a") (TYPE "int") (VARIABLE-VALUE (EXPRESSION-VALUE (TYPE "number") (VALUE "2")) (EXPRESSION-VALUE (TYPE "math-operator") (VALUE "+")) (EXPRESSION-VALUE (TYPE "number") (VALUE "3"))))))
(defparameter *code-test-expression-archimetic-simple*
" 
using UnityEngine;

public class FactoryEconomy{
      private int a = 2+3;
}
")

(defparameter *code-test-expression-archimetic-nested-tokens* (list "using" "UnityEngine" ";" "public" "class" "FactoryEconomy" "{" "private" "int" "a" "=" "2" "+" "(" "3" "+" "1" ")" "-" "5" ";" "private" "int" "b" "=" "10" ";" "}"))
(defparameter *code-test-expression-archimetic-nested-ast-tree* '((FILE "name") (USING "UnityEngine") (CLASS-DECLARATION (CLASS-VISIBILITY "public") (CLASS-NAME "FactoryEconomy")) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "a") (TYPE "int") (VARIABLE-VALUE (EXPRESSION-VALUE (TYPE "number") (VALUE "2")) (EXPRESSION-VALUE (TYPE "math-operator") (VALUE "+")) (NESTED-EXPRESSION (EXPRESSION-VALUE (TYPE "number") (VALUE "3")) (EXPRESSION-VALUE (TYPE "math-operator") (VALUE "+")) (EXPRESSION-VALUE (TYPE "number") (VALUE "1"))) (EXPRESSION-VALUE (TYPE "math-operator") (VALUE "-")) (EXPRESSION-VALUE (TYPE "number") (VALUE "5")))) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "b") (TYPE "int") (VARIABLE-VALUE (EXPRESSION-VALUE (TYPE "number") (VALUE "10"))))))
(defparameter *code-test-expression-archimetic-nested*
" 
using UnityEngine;

public class FactoryEconomy{
      private int a = 2+(3+1)-5;
      private int b = 10;
}
")

(defparameter *code-test-expression-archimetic-nested-minus-tokens* (list "using" "UnityEngine" ";" "public" "class" "FactoryEconomy" "{" "private" "int" "a" "=" "2" "+" "(" "-" "3" "+" "1" ")" "-" "5" ";" "private" "int" "b" "=" "10" ";" "}"))
(defparameter *code-test-expression-archimetic-nested-minus-ast-tree* '((FILE "name") (USING "UnityEngine") (CLASS-DECLARATION (CLASS-VISIBILITY "public") (CLASS-NAME "FactoryEconomy")) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "a") (TYPE "int") (VARIABLE-VALUE (EXPRESSION-VALUE (TYPE "number") (VALUE "2")) (EXPRESSION-VALUE (TYPE "math-operator") (VALUE "+")) (NESTED-EXPRESSION (EXPRESSION-VALUE (TYPE "math-operator") (VALUE "-")) (EXPRESSION-VALUE (TYPE "number") (VALUE "3")) (EXPRESSION-VALUE (TYPE "math-operator") (VALUE "+")) (EXPRESSION-VALUE (TYPE "number") (VALUE "1"))) (EXPRESSION-VALUE (TYPE "math-operator") (VALUE "-")) (EXPRESSION-VALUE (TYPE "number") (VALUE "5")))) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "b") (TYPE "int") (VARIABLE-VALUE (EXPRESSION-VALUE (TYPE "number") (VALUE "10"))))))
(defparameter *code-test-expression-archimetic-nested-minus*
" 
using UnityEngine;

public class FactoryEconomy{
      private int a = 2+(-3+1)-5;
      private int b = 10;
}
")

(defparameter *code-test-class-function-call-tokens* (list "using" "UnityEngine" ";" "using" "UnityEngine" ";" "partial" "public" "class" "FactoryEconomy" ":" "BuildingEconomy" "," "IGUI" "{" "private" "int" "a" "=" "1" ";" "public" "int" "add" "(" "int" "b" "," "int" "c" ")" "{" "var" "d" "=" "2" ";" "e" "(" "3" "," "4" "," "5" ")" ";" "return" "b" ";" "}" "private" "int" "f" "=" "6" ";" "}" ))
(defparameter *code-test-class-function-call-ast-tree* '((FILE "name") (USING "UnityEngine") (USING "UnityEngine") (CLASS-DECLARATION (CLASS-VISIBILITY "partial" "public") (CLASS-NAME "FactoryEconomy") (CLASS-INHERITANCES "BuildingEconomy" "IGUI")) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "a") (TYPE "int") (VARIABLE-VALUE (EXPRESSION-VALUE (TYPE "number") (VALUE "1")))) (FUNCTION-NODE (VISIBILITY "public") (TYPE "int") (FUNCTION-NAME "add") (FUNCTION-PARAMETERS (FUNCTION-PARAMETER (TYPE "int") (VARIABLE-NAME "b")) (FUNCTION-PARAMETER (TYPE "int") (VARIABLE-NAME "c"))) (FUNCTION-VARIABLE (VISIBILITY) (VARIABLE-NAME "d") (TYPE "var") (VARIABLE-VALUE (EXPRESSION-VALUE (TYPE "number") (VALUE "2")))) (FUNCTION-CALL (FUNCTION-CALL-NAME "e") (FUNCTION-ARGUMENTS ((EXPRESSION-VALUE (TYPE "number") (VALUE "3"))) ((EXPRESSION-VALUE (TYPE "number") (VALUE "4"))) ((EXPRESSION-VALUE (TYPE "number") (VALUE "5"))))) (FUNCTION-RETURN (IDENTIFIER "b"))) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "f") (TYPE "int") (VARIABLE-VALUE (EXPRESSION-VALUE (TYPE "number") (VALUE "6"))))))		 
(defparameter *code-test-class-function-call*
" 
using UnityEngine;
using UnityEngine;

partial public class FactoryEconomy : BuildingEconomy, IGUI {
      private int a = 1;
      public int add(int b, int c){
          var d = 2;
          e(3,4, 5);
          return b;
      }
      private int f = 6;
}
")

(defparameter *code-test-class-function-call-in-expression-tokens* (list "using" "UnityEngine" ";" "using" "UnityEngine" ";" "partial" "public" "class" "FactoryEconomy" ":" "BuildingEconomy" "," "IGUI" "{" "private" "int" "a" "=" "1" ";" "public" "int" "add" "(" "int" "b" "," "int" "c" ")" "{" "var" "d" "=" "2" ";" "var" "t" "=" "e" "(" "3" "," "4" "," "5" ")" ";" "return" "b" ";" "}" "private" "int" "f" "=" "6" ";" "}" ))
(defparameter *code-test-class-function-call-in-expression-ast-tree* '((FILE "name") (USING "UnityEngine") (USING "UnityEngine") (CLASS-DECLARATION (CLASS-VISIBILITY "partial" "public") (CLASS-NAME "FactoryEconomy") (CLASS-INHERITANCES "BuildingEconomy" "IGUI")) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "a") (TYPE "int") (VARIABLE-VALUE (EXPRESSION-VALUE (TYPE "number") (VALUE "1")))) (FUNCTION-NODE (VISIBILITY "public") (TYPE "int") (FUNCTION-NAME "add") (FUNCTION-PARAMETERS (FUNCTION-PARAMETER (TYPE "int") (VARIABLE-NAME "b")) (FUNCTION-PARAMETER (TYPE "int") (VARIABLE-NAME "c"))) (FUNCTION-VARIABLE (VISIBILITY) (VARIABLE-NAME "d") (TYPE "var") (VARIABLE-VALUE (EXPRESSION-VALUE (TYPE "number") (VALUE "2")))) (FUNCTION-VARIABLE (VISIBILITY) (VARIABLE-NAME "t") (TYPE "var") (VARIABLE-VALUE (FUNCTION-CALL (FUNCTION-CALL-NAME "e") (FUNCTION-ARGUMENTS ((EXPRESSION-VALUE (TYPE "number") (VALUE "3"))) ((EXPRESSION-VALUE (TYPE "number") (VALUE "4"))) ((EXPRESSION-VALUE (TYPE "number") (VALUE "5"))))))) (FUNCTION-RETURN (IDENTIFIER "b"))) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "f") (TYPE "int") (VARIABLE-VALUE (EXPRESSION-VALUE (TYPE "number") (VALUE "6"))))))
(defparameter *code-test-class-function-call-in-expression*
" 
using UnityEngine;
using UnityEngine;

partial public class FactoryEconomy : BuildingEconomy, IGUI {
      private int a = 1;
      public int add(int b, int c){
          var d = 2;
          var t = e(3,4, 5);
          return b;
      }
      private int f = 6;
}
")

(defparameter *code-test-class-function-call-in-expression-params-expression-tokens* (list "using" "UnityEngine" ";" "using" "UnityEngine" ";" "partial" "public" "class" "FactoryEconomy" ":" "BuildingEconomy" "," "IGUI" "{" "private" "int" "a" "=" "1" ";" "public" "int" "add" "(" "int" "b" "," "int" "c" ")" "{" "var" "d" "=" "2" ";" "var" "t" "=" "e" "(" "3" "+" "5" "," "a" "," "c" "-" "d" ")" ";" "return" "b" ";" "}" "private" "int" "f" "=" "6" ";" "}"))
(defparameter *code-test-class-function-call-in-expression-params-expression-ast-tree* '((FILE "name") (USING "UnityEngine") (USING "UnityEngine") (CLASS-DECLARATION (CLASS-VISIBILITY "partial" "public") (CLASS-NAME "FactoryEconomy") (CLASS-INHERITANCES "BuildingEconomy" "IGUI")) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "a") (TYPE "int") (VARIABLE-VALUE (EXPRESSION-VALUE (TYPE "number") (VALUE "1")))) (FUNCTION-NODE (VISIBILITY "public") (TYPE "int") (FUNCTION-NAME "add") (FUNCTION-PARAMETERS (FUNCTION-PARAMETER (TYPE "int") (VARIABLE-NAME "b")) (FUNCTION-PARAMETER (TYPE "int") (VARIABLE-NAME "c"))) (FUNCTION-VARIABLE (VISIBILITY) (VARIABLE-NAME "d") (TYPE "var") (VARIABLE-VALUE (EXPRESSION-VALUE (TYPE "number") (VALUE "2")))) (FUNCTION-VARIABLE (VISIBILITY) (VARIABLE-NAME "t") (TYPE "var") (VARIABLE-VALUE (FUNCTION-CALL (FUNCTION-CALL-NAME "e") (FUNCTION-ARGUMENTS ((EXPRESSION-VALUE (TYPE "number") (VALUE "3")) (EXPRESSION-VALUE (TYPE "math-operator") (VALUE "+")) (EXPRESSION-VALUE (TYPE "number") (VALUE "5"))) ((IDENTIFIER "a")) ((IDENTIFIER "c") (EXPRESSION-VALUE (TYPE "math-operator") (VALUE "-")) (IDENTIFIER "d")))))) (FUNCTION-RETURN (IDENTIFIER "b"))) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "f") (TYPE "int") (VARIABLE-VALUE (EXPRESSION-VALUE (TYPE "number") (VALUE "6"))))))
(defparameter *code-test-class-function-call-in-expression-params-expression*
" 
using UnityEngine;
using UnityEngine;

partial public class FactoryEconomy : BuildingEconomy, IGUI {
      private int a = 1;
      public int add(int b, int c){
          var d = 2;
          var t = e(3+5,a, c-d);
          return b;
      }
      private int f = 6;
}
")


(defun test-tokenizer-simple (test-string control-tokens)
  (tokenizer::match-token-list (tokenize-csharp-code test-string) control-tokens))

(defun run-tokenizer-test-suite-csharp ()
  (let* ((t-fn #'test-tokenizer-simple)
	 (t-test (lambda (test-string control-tokens)
		   (handler-case (funcall t-fn test-string control-tokens)
		     (token-mismatch-error (condition)
		       (format t "~S" (tokenizer::text condition)) nil))))) ;; test-function
    (and (funcall t-test *code-test-class* *code-test-class-tokens*)
	 (funcall t-test *code-test-variables-simple* *code-test-variables-simple-tokens*)
	 (funcall t-test *code-test-variables-custom-type* *code-test-variables-custom-type-tokens*)
	 (funcall t-test *code-test-variables-arrays* *code-test-variables-arrays-tokens*)
	 (funcall t-test *code-test-class-function* *code-test-class-function-tokens*)
	 (funcall t-test *code-test-expression-archimetic-simple* *code-test-expression-archimetic-simple-tokens*)
	 (funcall t-test *code-test-expression-archimetic-nested* *code-test-expression-archimetic-nested-tokens*)
	 (funcall t-test *code-test-expression-archimetic-nested-minus* *code-test-expression-archimetic-nested-minus-tokens*)
	 (funcall t-test *code-test-class-function-call* *code-test-class-function-call-tokens*)
	 (funcall t-test *code-test-class-function-call-in-expression* *code-test-class-function-call-in-expression-tokens*)
	 (funcall t-test *code-test-class-function-call-in-expression-params-expression* *code-test-class-function-call-in-expression-params-expression-tokens*))))

(defun run-ast-test-suite-csharp ()
  (let* ((parse-code (lambda (code) (parse-csharp(tokenize-csharp-code code))))
	 (test-ast-tree (lambda (source test-tree) (test-ast-tree (funcall parse-code source)
								  test-tree)))
	 (t-test (lambda (test-string control-tokens)
		   (handler-case (funcall test-ast-tree test-string control-tokens)
		     (ast-node-mismatch-error (condition)
		       (format t "~S" (ast-node-space::text condition)) nil)))))
    (and (funcall t-test *code-test-class* *code-test-class-ast-tree*)
	 (funcall t-test *code-test-variables-simple* *code-test-variables-simple-ast-tree*)
	 (funcall t-test *code-test-variables-custom-type* *code-test-variables-custom-type-ast-tree* )
	 (funcall t-test *code-test-variables-arrays* *code-test-variables-arrays-ast-tree* )
	 (funcall t-test *code-test-class-function* *code-test-class-function-ast-tree*)
	 (funcall t-test *code-test-expression-archimetic-simple* *code-test-expression-archimetic-simple-ast-tree*)
	 (funcall t-test *code-test-expression-archimetic-nested* *code-test-expression-archimetic-nested-ast-tree*)
	 (funcall t-test *code-test-expression-archimetic-nested-minus* *code-test-expression-archimetic-nested-minus-ast-tree*)
	 (funcall t-test *code-test-class-function-call* *code-test-class-function-call-ast-tree*)
	 (funcall t-test *code-test-class-function-call-in-expression* *code-test-class-function-call-in-expression-ast-tree*)
	 (funcall t-test *code-test-class-function-call-in-expression-params-expression* *code-test-class-function-call-in-expression-params-expression-ast-tree*))))

"

      private List<int> _playerGameObjectList;
      bool _haveConceded = false;
      private int a = 3;
      private int b = 3;
      var b = 2;
"

