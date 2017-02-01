(ql:quickload "cl-utilities")

(defpackage :csharp-parser
  (:use :common-lisp :tokenizer))
(in-package :csharp-parser)

;;;; Test Area
;; (defparameter test (parse-c-sharp-fn-basic (tokenize-c-sharp-fn *code-test*)))
(defun make-string-readable (string) (concatenate 'string "(" string ")" ))
(defparameter *code-test* "public void setName  ( int id,bool employee ){ 
 var _a = 2;
var _employee = employee;
  
 } ")
(defparameter *code-test-adv* 
"var _aa = 22; public void setName(int id, bool employee ,STRING BLA){
  var _a = 2;
  var _employee = employee;
  var _BLA = BLA;
}

public void dummy(int test2){
var _test2 = test2;
}
")
(read-from-string
"public void setName  ( int id,bool employee ,string newName ){ 
 var _ = 2;
var _newName = newName;
  
 } " )

;;;; Parse Area
(defun tokenize-with-symbols (string-to-tokenize)
  (tokenizer:tokenize-fn string-to-tokenize
   (tokenizer:add-length-to-strings '("public"
     "private"
     "protected"
     ";"
     "("
     ")"
     "{"
     "}"
     ",")))) 

(defun parse-c-sharp-fn-basic (token-string-list)
  (parse-c-sharp-fn-basic-def token-string-list))

(defun parse-c-sharp-fn-basic-def (token-string-list)
  (cons (list (nth 0 token-string-list) (nth 1 token-string-list) (nth 2 token-string-list)) 
	(parse-c-sharp-fn-basic-paras (nthcdr 3 token-string-list))))

(defun build-param-list (token-string-list new-list)
  (if (string-equal ")" (car token-string-list))
      (cons (nreverse new-list )
	    (parse-c-sharp-fn-basic-body (cdr token-string-list))) 
      (let ((current-token (cadr token-string-list))
	    (next-tokens (cddr token-string-list)))
	(build-param-list (nthcdr 3 token-string-list) 
			    (push (list current-token
					(car next-tokens))
				  new-list )))))

(defun parse-c-sharp-fn-basic-paras (token-string-list)
  (build-param-list token-string-list
		    (list )))

(defun parse-c-sharp-fn-basic-body (token-string-list) 
  (when (not(string-equal (car token-string-list) "{")) (error "body does not begin with {"))
  (list 
   (list-stmts (cdr token-string-list))))

(defun list-stmts (token-string-list)
  (let ((stmts-list ())
	(stmt ()))
    (dolist (token token-string-list) 
      (if (string-equal token ";")
	  (progn 
	    (push (nreverse stmt) stmts-list)
	    (setf stmt ()))
	  (push token stmt)))
  (nreverse stmts-list)))
