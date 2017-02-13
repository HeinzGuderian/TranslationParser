(ql:quickload "cl-utilities")

(defpackage :tokenizer
  (:use :common-lisp)
  (:export :tokenize-fn :add-length-to-strings :tokenizer-object))
(in-package :tokenizer)
;;(tokenize-c-sharp-fn *code-test*)

(defparameter *code-test-csharp* "public void setName  ( int id,bool employee ){ 
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

;;'("public" "private" "protected" ";" "(" ")" "{" "}" ",")
(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))
(abbrev split cl-utilities:split-sequence )
(abbrev split-if cl-utilities:split-sequence-if )
(abbrev dbind destructuring-bind)

(defmacro add-length-to-strings (string-list)
  `(mapcar #'(lambda (x) (cons x (length x)))
	,string-list))

(defun get-token-name (token-struct)  (car token-struct))
(defun get-token-length (token-struct)  (cdr token-struct))
(defun get-token (token-struct) (car token-struct))
(defun get-next-token (token-struct) (cdr token-struct))
(defun tokens-length (token-struct) (length token-struct))
(defun find-token (string-to-find token-struct)
  (dolist (token-part token-struct)
    (let ((is-found (search (get-token-name token-part) string-to-find )))
    (when is-found
      (return (values is-found (+ is-found(get-token-length token-part))))))))

(defun tokenize-fn (string-to-tokenize get-tokens)
  (let ((rougly-splitted (split-if (lambda (x) (or (equal x #\Newline)(equal x #\Space))) string-to-tokenize :remove-empty-subseqs t))
	(new-list ()))
    (labels ((add-new-token (string-part)  
	       ;;(labels ((push-and-add (string-part )))
	       (multiple-value-bind (start-index last-index) (find-token string-part get-tokens)
		 (if (and(or start-index last-index)) ;found-token
		     (if (eq start-index 0) 
			 (progn 
			   (push (subseq string-part 0 last-index) new-list)
			   (when (not(eq last-index (length string-part)))(add-new-token (subseq string-part last-index))))
			 (progn
			   (push (subseq string-part 0 start-index) new-list)
			   (add-new-token (subseq string-part start-index))))
		     (push string-part new-list))))
	     (is-empty-new-line (string-to-test) (string-equal "" (string-trim '(#\Space #\Newline) string-to-test))))
      (dolist (string-part rougly-splitted)
	(when (not (is-empty-new-line string-part)) (add-new-token (string-trim '(#\Space #\Newline) string-part))))
      (nreverse new-list))))

(defun tokenizer-object (token-list-org)
  (let ((token-list token-list-org))
    (flet ((current-token ()
	     (car token-list))
	   (peek-token ()
	     (cadr token-list))
	   (advanze-token ()
	     (setf token-list (cdr token-list))
	     (car token-list)))
    (lambda (cmd)
      (cond ((eq :current cmd) (current-token))
	    ((eq :peek cmd) (peek-token))
	    ((eq :advanze cmd) (advanze-token))
	    (t (current-token)))))))
