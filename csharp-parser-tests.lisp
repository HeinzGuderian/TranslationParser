(in-package :csharp-parser) 

(declaim (optimize (debug 3)))

;; (in-package :csharp-parser)
;; (defparameter test (parse-csharp (tokenize-csharp-code *code-test-class*)))
;; command for generating test matches (print-tokens (tokenize-csharp-code *code-test-variables-advanced* ))
;; (print-tokens (tokenize-csharp-code *code-test-variables-simple* ))
;; (parse-csharp (tokenize-csharp-code *code-test-variables-simple*))

;;(match-shallow-ast-node (cadr(cadddr(parse-csharp(tokenize-csharp-code *code-test-variables-advanced*)))) '(class-visibility "partial" "public"))

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

(defparameter *code-test-variables-simple-ast-tree* '((FILE "name") (USING "UnityEngine") (USING "UnityEngine") (CLASS-DECLARATION (CLASS-VISIBILITY "partial" "public") (CLASS-NAME "FactoryEconomy") (CLASS-INHERITANCES "BuildingEconomy" "IGUI")) (CLASS-VARIABLE (VISIBILITY) (VARIABLE-NAME "c") (TYPE "int") (VARIABLE-VALUE)) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "b") (TYPE "int") (VARIABLE-VALUE (EXPRESSION-VALUE (TYPE "number") (VALUE "2")))) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "a") (TYPE "int") (VARIABLE-VALUE))))

;;(test-ast-tree (parse-csharp(tokenize-csharp-code *code-test-variables-simple*)) '((FILE "name") (USING "UnityEngine") (USING "UnityEngine") (CLASS-DECLARATION (CLASS-VISIBILITY "partial" "public") (CLASS-NAME "FactoryEconomy") (CLASS-INHERITANCES "BuildingEconomy" "IGUI")) (CLASS-VARIABLE (VISIBILITY) (VARIABLE-NAME "c") (TYPE "int") (VALUE)) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "a") (TYPE "int") (VALUE)) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "b") (TYPE "int") (VALUE "2"))))

;;(walk-collect-all-ast-nodes '((FILE "name") (USING "UnityEngine") (USING "UnityEngine") (CLASS-DECLARATION (CLASS-VISIBILITY "partial" "public") (CLASS-NAME "FactoryEconomy") (CLASS-INHERITANCES "BuildingEconomy" "IGUI")) (CLASS-VARIABLE (VISIBILITY) (VARIABLE-NAME "c") (TYPE "int") (VALUE)) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "a") (TYPE "int") (VALUE)) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "b") (TYPE "int") (VALUE "2")))) 
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
	 (funcall t-test *code-test-expression-archimetic-nested* *code-test-expression-archimetic-nested-tokens*))))

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
	 (funcall t-test *code-test-expression-archimetic-nested* *code-test-expression-archimetic-nested-ast-tree*))))

"

      private List<int> _playerGameObjectList;
      bool _haveConceded = false;
      private int a = 3;
      private int b = 3;
      var b = 2;
"

