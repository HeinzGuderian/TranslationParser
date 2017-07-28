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

(defparameter *code-test-variables-simple-tokens* (list "using" "UnityEngine" ";" "using" "UnityEngine" ";" "partial" "public" "class" "FactoryEconomy" ":" "BuildingEconomy" "," "IGUI" "{" "int" "c" ";" "private" "int" "a" ";" "private" "int" "b" "=" "2" ";" "}" ))

(defparameter *code-test-variables-simple-ast-tree* '((FILE "name") (USING "UnityEngine") (USING "UnityEngine") (CLASS-DECLARATION (CLASS-VISIBILITY "partial" "public") (CLASS-NAME "FactoryEconomy") (CLASS-INHERITANCES "BuildingEconomy" "IGUI")) (CLASS-VARIABLE (VISIBILITY) (VARIABLE-NAME "c") (TYPE "int") (VALUE)) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "a") (TYPE "int") (VALUE)) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "b") (TYPE "int") (VALUE "2"))))

;;(test-ast-tree (parse-csharp(tokenize-csharp-code *code-test-variables-simple*)) '((FILE "name") (USING "UnityEngine") (USING "UnityEngine") (CLASS-DECLARATION (CLASS-VISIBILITY "partial" "public") (CLASS-NAME "FactoryEconomy") (CLASS-INHERITANCES "BuildingEconomy" "IGUI")) (CLASS-VARIABLE (VISIBILITY) (VARIABLE-NAME "c") (TYPE "int") (VALUE)) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "a") (TYPE "int") (VALUE)) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "b") (TYPE "int") (VALUE "2"))))

;;(walk-collect-all-ast-nodes '((FILE "name") (USING "UnityEngine") (USING "UnityEngine") (CLASS-DECLARATION (CLASS-VISIBILITY "partial" "public") (CLASS-NAME "FactoryEconomy") (CLASS-INHERITANCES "BuildingEconomy" "IGUI")) (CLASS-VARIABLE (VISIBILITY) (VARIABLE-NAME "c") (TYPE "int") (VALUE)) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "a") (TYPE "int") (VALUE)) (CLASS-VARIABLE (VISIBILITY "private") (VARIABLE-NAME "b") (TYPE "int") (VALUE "2")))) 
(defparameter *code-test-variables-simple* 
" 
using UnityEngine;
using UnityEngine;

partial public class FactoryEconomy : BuildingEconomy, IGUI 
{
 int c;
 private int a;
 private int b = 2;
}"
  )

(defparameter *code-test-variables-advanced-tokens*  (list "using" "UnityEngine" ";" "using" "UnityEngine" ";" "partial" "public" "class" "FactoryEconomy" ":" "BuildingEconomy" "," "IGUI" "{" "public" "GameObject" "_playerGameObject" ";" "TeamScript.PlayerNumberEnum" "_winningPlayer" ";" "private" "int" "[" "]" "d" ";" "}"))

(defparameter *code-test-variables-advanced* 
" 
using UnityEngine;
using UnityEngine;

partial public class FactoryEconomy : BuildingEconomy, IGUI {
      public GameObject _playerGameObject;
      TeamScript.PlayerNumberEnum _winningPlayer;
      private int[] d;
}
")

(defun test-tokenizer-simple (test-string control-tokens)
  (tokenizer::match-token-list (tokenize-csharp-code test-string) control-tokens))

(defun run-tokenizer-test-suite-csharp ()
  (let ((t-fn #'test-tokenizer-simple)) ;; test-function
    (and (funcall t-fn *code-test-class* *code-test-class-tokens*)
	 (funcall t-fn *code-test-variables-simple* *code-test-variables-simple-tokens*)
	 (funcall t-fn *code-test-variables-advanced* *code-test-variables-advanced-tokens*))))

(defun run-ast-test-suite-csharp ()
  (let* ((parse-code (lambda (code) (parse-csharp(tokenize-csharp-code code))))
	 (test-ast-tree (lambda (source test-tree) (test-ast-tree (funcall parse-code source)
								  test-tree))))
    (and (funcall test-ast-tree *code-test-class* *code-test-class-ast-tree*))))

"

      private List<int> _playerGameObjectList;
      bool _haveConceded = false;
      private int a = 3;
      private int b = 3;
      var b = 2;
"

