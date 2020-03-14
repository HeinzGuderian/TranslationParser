(in-package :csharp-parser) 
(declaim (optimize (debug 3)))
;; (in-package :csharp-parser)
;; (defparameter test (parse-csharp (tokenize-csharp-code *code-test-class*)))
;; command for generating test matches (print-tokens (tokenize-csharp-code *code-test-variables-advanced* ))
;; (print-tokens (tokenize-csharp-code *code-test-variables-simple* ))
;; (parse-csharp (tokenize-csharp-code *code-test-variables-simple*))
;;(match-shallow-ast-node (cadr(cadddr(parse-csharp(tokenize-csharp-code *code-test-variables-advanced*)))) '(class-visibility "partial" "public"))
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
(defmacro test-and-set (place test node)
  `(when (not,place)
     (setf ,place (funcall ,test ,node))))
(defun test-node (node-identifier-fn node-test)
  (lambda (node)
    (and (funcall node-identifier-fn node)
	 (funcall node-test node))))
(defun test-some-subnode (test)
  (lambda (node) (some test (subnodes node))))
(defun test-car-data (test-data &optional (test-fn #'equal))
  (lambda (node) (funcall test-fn test-data (car (data-from-ast-node node)))))
(defun test-data (test-data &optional (test-fn #'equal))
  (lambda (node) (funcall test-fn test-data (data-from-ast-node node))))
(defun **Macro-template-function** ()
  (with-is-symbol ((variable-sym? "variable-name")
		   (class-variable-sym? "class-variable"))
    (let ((test1 (gensym))
	  (test2 (gensym)))
      (lambda (node) 
	(test-and-set test1 (test-node class-variable-sym?
				       (test-some-subnode (test-node variable-sym? (test-car-data "a")))) node)
	(test-and-set test2 (test-node class-variable-sym?
				       (test-some-subnode (test-node variable-sym? (test-car-data "b")))) node)
	(and test1 test2)))))

;;(funcall (exist-node-in-tree (test1111)) (parse-csharp (tokenize-csharp-code *code-test-variables-simple*)))
(defmacro create-test-defun (name variables-check tests)
  (let ((symbols (mapcar #'(lambda (pair) (declare (ignore pair)) (gensym)) tests)))
    `(defun ,name ()
       (let (,@symbols)
	 (with-is-symbol ,variables-check
	   (lambda (node)
	     ,@(mapcar #'(lambda (test symbol) 
			   `(test-and-set ,symbol ,test node))
		       tests
		       symbols)
	     (and ,@(mapcar (lambda (sym) sym)
			    symbols))))))))

(defparameter *code-test-class-tokens*
  (list "using" "UnityEngine" ";" "using" "UnityEngine" ";" "partial" "public"
	"class" "FactoryEconomy" ":" "BuildingEconomy" "," "IGUI" "{" "}"))
(defparameter *code-test-class* 
" 
using UnityEngine;
using UnityEngine;

partial public class FactoryEconomy : BuildingEconomy, IGUI {}")
(create-test-defun *code-test-class-ast-tree*
		   ((class-sym? "class-declaration") (class-visibility-sym? "class-visibility")
		    (class-name-sym? "class-name") (class-inheritances-sym? "class-inheritances"))
		   ((test-node class-sym?
			       (test-some-subnode
				(test-node class-visibility-sym?
					   (test-data (list "partial" "public")))))
		    (test-node class-sym?
			       (test-some-subnode
				(test-node class-name-sym?
					   (test-car-data "FactoryEconomy"))))
		    (test-node class-sym?
			       (test-some-subnode
				(test-node class-inheritances-sym?
					   (test-data (list "BuildingEconomy" "IGUI")))))))

(defparameter *code-test-variables-simple-tokens*
  (list "using" "UnityEngine"
	";" "using" "UnityEngine" ";" "partial" "public" "class" "FactoryEconomy" ":" "BuildingEconomy"
	"," "IGUI" "{" "int" "c" ";" "private" "int" "b" "=" "2" ";" "private" "int" "a" ";" "}" ))
(defparameter *code-test-variables-simple* 
" 
using UnityEngine;
using UnityEngine;

partial public class FactoryEconomy : BuildingEconomy, IGUI 
{
int c;
private int b = 2;
private int a;
}")
(create-test-defun *code-test-variables-simple-ast-tree*
		   ((variable-sym? "variable-name") (class-variable-sym? "class-variable"))
		   ((test-node class-variable-sym?
			       (test-some-subnode
				(test-node variable-sym?
					   (test-car-data "c"))))
		    (test-node class-variable-sym?
			       (test-some-subnode
				(test-node variable-sym?
					   (test-car-data "b"))))
		    (test-node class-variable-sym?
			       (test-some-subnode
				(test-node variable-sym?
					   (test-car-data "c"))))))

(defparameter *code-test-variables-custom-type-tokens*
  (list "using" "UnityEngine" ";" "using" "UnityEngine" ";" "partial" "public" "class" "FactoryEconomy"
	":" "BuildingEconomy" "," "IGUI" "{" "public" "GameObject" "_playerGameObject" ";"
	"TeamScript.PlayerNumberEnum" "_winningPlayer" ";" "private" "int" "a" "=" "2" ";" "}"))
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
(create-test-defun *code-test-variables-custom-type-ast-tree*
		   ((variable-type-sym? "type") (class-variable-sym? "class-variable"))
		   ((test-node class-variable-sym?
			       (test-some-subnode
				(test-node variable-type-sym?
					   (test-car-data "GameObject"))))))

(defparameter *code-test-variables-arrays-tokens*
  (list "using" "UnityEngine" ";" "using" "UnityEngine" ";" "partial" "public" "class"
	"FactoryEconomy" ":" "BuildingEconomy" "," "IGUI" "{"
	"private" "int" "[" "]" "d" ";" "private" "int" "a" "=" "2" ";" "}"))
(defparameter *code-test-variables-arrays* 
" 
using UnityEngine;
using UnityEngine;

partial public class FactoryEconomy : BuildingEconomy, IGUI {
  private int[] d;
  private int a = 2;
}
")
(create-test-defun *code-test-variables-arrays-ast-tree*
	       ((variable-type-sym? "type") (class-variable-sym? "class-variable"))
	       ((test-node class-variable-sym?
			   (test-some-subnode
			    (test-node variable-type-sym?
				       (test-car-data "int[]"))))))

(defparameter *code-test-class-function-tokens*
  (list "using" "UnityEngine" ";" "using" "UnityEngine" ";" "partial" "public" "class" "FactoryEconomy" ":"
	"BuildingEconomy" "," "IGUI" "{" "private" "int" "a" "=" "2" ";" "public" "int" "add"
	"(" "int" "b" "," "int" "c" ")" "{" "var" "d" "=" "3" ";" "return" "b" ";" "}"
	"private" "int" "e" "=" "5" ";" "}" ))
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
(create-test-defun *code-test-class-function-ast-tree*
		   ((function-sym? "function-node") (function-name-sym? "function-name")
		    (function-declaration? "function-declaration"))
		   ((test-node function-sym?
			       (test-some-subnode
				(test-node function-declaration?
					   (test-some-subnode
					    (test-node function-name-sym?
						       (test-car-data "add"))))))))

(defparameter *code-test-expression-archimetic-simple-tokens*
  (list "using" "UnityEngine" ";" "public" "class" "FactoryEconomy"
	"{" "private" "int" "a" "=" "2" "+" "3" ";" "}"))
(defparameter *code-test-expression-archimetic-simple*
" 
using UnityEngine;

public class FactoryEconomy{
  private int a = 2+3;
}
")

(defparameter *code-test-expression-archimetic-nested-tokens*
  (list "using" "UnityEngine" ";" "public" "class" "FactoryEconomy"
	"{" "private" "int" "a" "=" "2" "+" "(" "3" "+" "1" ")" "-" "5"
	";" "private" "int" "b" "=" "10" ";" "}"))
(defparameter *code-test-expression-archimetic-nested*
" 
using UnityEngine;

public class FactoryEconomy{
  private int a = 2+(3+1)-5;
  private int b = 10;
}
")

(defparameter *code-test-expression-archimetic-nested-minus-tokens*
  (list "using" "UnityEngine" ";" "public" "class" "FactoryEconomy"
	"{" "private" "int" "a" "=" "2" "+" "(" "-" "3" "+" "1" ")" "-" "5" ";"
	"private" "int" "b" "=" "10" ";" "}"))
(defparameter *code-test-expression-archimetic-nested-minus*
" 
using UnityEngine;

public class FactoryEconomy{
  private int a = 2+(-3+1)-5;
  private int b = 10;
}
")

(defparameter *code-test-class-function-call-tokens*
  (list "using" "UnityEngine" ";" "using" "UnityEngine" ";" "partial" "public" "class"
	"FactoryEconomy" ":" "BuildingEconomy" "," "IGUI" "{" "private" "int" "a" "=" "1"
	";" "public" "int" "add" "(" "int" "b" "," "int" "c" ")" "{" "var" "d" "=" "2" ";"
	"e" "(" "3" "," "4" "," "5" ")" ";" "return" "b" ";" "}" "private" "int" "f" "=" "6" ";" "}" ))		 
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

(defparameter *code-test-class-function-call-in-expression-tokens*
  (list "using" "UnityEngine" ";" "using" "UnityEngine" ";" "partial" "public" "class" "FactoryEconomy"
	":" "BuildingEconomy" "," "IGUI" "{" "private" "int" "a" "=" "1" ";" "public" "int" "add"
	"(" "int" "b" "," "int" "c" ")" "{" "var" "d" "=" "2" ";" "var" "t" "=" "e" "(" "3" "," "4" "," "5" ")" ";"
	"return" "b" ";" "}" "private" "int" "f" "=" "6" ";" "}" ))
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

(defparameter *code-test-class-function-call-in-expression-params-expression-tokens*
  (list "using" "UnityEngine" ";" "using" "UnityEngine" ";" "partial" "public" "class" "FactoryEconomy" ":"
	"BuildingEconomy" "," "IGUI" "{" "private" "int" "a" "=" "1" ";" "public" "int" "add" "(" "int" "b" "," "int"
	"c" ")" "{" "var" "d" "=" "2" ";" "var" "t" "=" "e" "(" "3" "+" "5" "," "a" "," "c" "-" "d" ")" ";"
	"return" "b" ";" "}" "private" "int" "f" "=" "6" ";" "}"))
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
	 (test-ast-tree (lambda (test-fn) (exist-node-in-tree (funcall test-fn))))
	 (t-test (lambda (test-fn parsed-code)
		   (handler-case (funcall (funcall test-ast-tree test-fn) (funcall parse-code parsed-code))
		     (ast-node-mismatch-error (condition)
		       (format t "~S" (ast-node-space::text condition)) nil)))))
    (and (funcall t-test #'*code-test-class-ast-tree* *code-test-class*)
	 (funcall t-test #'*code-test-variables-simple-ast-tree* *code-test-variables-simple*)
	 (funcall t-test #'*code-test-variables-custom-type-ast-tree* *code-test-variables-custom-type*)
	 (funcall t-test #'*code-test-variables-arrays-ast-tree* *code-test-variables-arrays*)
	 (funcall t-test #'*code-test-class-function-ast-tree* *code-test-class-function*))))
