

;; Goal state
;; Add function to class
;; (fnPattern (public void setClassName) ( (int id) (string name) ) ((settAtt ,name "id.ToString();")) )
;; (onClass testClass settName)
;; (fn-def-pattern ("public" "void" "setName") ("string" "newName") (("int" "id")("bool" "employee")) c-name)
;;(fn-def-pattern ("public" "void" "setName") ("string" "newName") (("int" "id")("bool" "employee"))  (list "var _ = 2;"))
;; (fn-def-pattern ("public" "void" "setName") ("string" "newName") (("int" "id")("bool" "employee"))  (list "var _ = 2;" (fn-body-add-setter-fn fn-new-para-name)))
;; (fn-def-pattern ("public" "void" "setName") ("string" "newName") (list (list "int" "id")(list "bool" "employee"))  (list "var _ = 2;" (fn-body-add-setter-fn fn-new-para-name)))
(ql:quickload "cl-utilities")
(declaim (optimize (speed 0) (space 0) (debug 3)))


(defmacro fn-def-pattern ((c-visibility c-ret-type c-name) (fn-new-para-type fn-new-para-name)  fnParams &body body)
  `(let ((c-name ,c-name)
	 (fn-new-para-name ,fn-new-para-name))
     (format nil "~a ( ~a ~a ){ ~% ~{~a~&~}  ~& } " 
	     (format nil "~a ~a ~a " ,c-visibility ,c-ret-type ,c-name )
	     (format nil "~{~{~a ~a~}~^, ~}" ,fnParams ) ;;  '((int id)(string name))
	     (format nil ",~a ~a" ,fn-new-para-type ,fn-new-para-name)
	     ;;(list c-name fn-new-para-name)
	     ,(cadar body);;,@body;;,(cadar body)
	     ;;(fn-body-add-setter-fn fn-new-para-name)
	     )))
;;(defmacro fn-def-pattern-list (fn-list))
;;  `(fn-def-pattern ()))
;;(macro-test-1 (list (list 1 2 3) (list 2) (list 3)))
;;(fn-def-pattern-list (list (list "public" "void" "setName") (list "string" "newName") (list (list "int" "id"))))
;;(fn-def-pattern-list (list (list "public" "void" "setName") (list "string" "newName") (list (list "int" "id")) '(list "var _a = 2;" (fn-body-add-setter-fn fn-new-para-name))))
;;(let ((fn-list (list (list "public" "void" "setName") (list "string" "newName") (list (list "int" "id")(list "bool" "employee")) '(list "var _ = 2;" (fn-body-add-setter-fn fn-new-para-name))))) (fn-def-pattern-list fn-list))
(defmacro fn-def-pattern-list (fn-list)
    `(fn-def-pattern  ((nth 0 (car ,fn-list)) (nth 1 (car ,fn-list)) (nth 2 (car ,fn-list)))
	 ((nth 0 (cadr ,fn-list)) (nth 1 (cadr ,fn-list)))
	 (nth 2 ,fn-list);;(("int" "id")("bool" "employee")) 
       '(list "var _a = 2;" (fn-body-add-setter-fn fn-new-para-name))));;`( ,(nth 3 fn-list))))));;(nth 3 ,fn-list)));;(list "var _a = 2;" (fn-body-add-setter-fn fn-new-para-name)))))

(defmacro macro-test-1 (fn-list)
  (format t  "~a"  `',(car `,fn-list))
  (format t  "~a"  `,(+ 1 2))
  `(macro-test-2  ( (nth 0 (car ,fn-list)) (nth 1 (car ,fn-list)) (nth 2 (car ,fn-list)))))

(defmacro macro-test-2 ((a b c))
  `(+  ,a ,b ,c))

(defmacro macro-test-3 (fn-list)
  `(macro-test-4 (,@fn-list) ))

(defmacro macro-test-4 ((a b c))
  `(+  ,a ,b ,c))

(defun c-sharp-fn-def (c-visibility c-ret-type c-name)
  (format nil "~a ~a ~a " c-visibility c-ret-type c-name ))

(defmacro fn-body-add-setter ( &body body )
  `'( ,@body))

(defun fn-body-add-setter-fn ( var-name )
  (format nil "var _~a = ~a;" var-name var-name) )

(defun fn-body-add-setter-fn-test ( var-class var-name body-list )
  (list(format nil "~{~a~}~&var _~a = ~a;" body-list var-name var-name) ))

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(set-dispatch-macro-character #\# #\?
			      #'(lambda (stream char1 char2)
				  `#'(lambda (&rest ,(gensym))
				       ,(read stream t nil t))))

;;> (mapcar #?2 '(a b c))
;;(222)

(defmacro defdelim (left right parms &body body )
  `(ddfn ,left ,right #'(lambda ,parms ,@body) ))

(let ((rpar (get-macro-character #\) )))
  (defun ddfn (left right fn)
    (set-macro-character right rpar)
    (set-dispatch-macro-character #\# left
				  #'(lambda (stream char1 char2)
				      (apply fn
					     (read-delimited-list right stream t))))))

;;(defdelim #\[ #\] (x y)  (list 'quote (mapa-b #'identity (ceiling x) (floor y))))
;;(defdelim #\C #\D (rest)  
;;	  (list 'quote  (cl-utilities:SPLIT-SEQUENCE #\Space rest))
;;	  #\#)

(defun make-string-readable (string) (concatenate 'string "(" string ")" ))
(defvar *code-test* "public void setName  ( int id,bool employee ,string newName ){ 
 var _ = 2;
var _newName = newName;
  
 } ")

(read-from-string
"public void setName  ( int id,bool employee ,string newName ){ 
 var _ = 2;
var _newName = newName;
  
 } " )

(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))
(abbrev split cl-utilities:split-sequence )
(abbrev split-if cl-utilities:split-sequence-if )
(abbrev dbind destructuring-bind)

;; Visibility macros
(defmacro public (ret-type name)
  `'( "public" ,(symbol-name ret-type)  ,(symbol-name name )))

(defun parse-c-sharp (string-content)
  (split #\Space string-content))

(defun parse-c-sharp-fn-old (fn-string-list)
  (dbind (visibility ret-type fn-name &rest rest) fn-string-list
	 (list (list visibility ret-type fn-name) 
	       rest)))

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

(defmacro add-length-to-strings (string-list)
  `(mapcar #'(lambda (x) (cons x (length x)))
	,string-list))
(defparameter *token-list*
   (add-length-to-strings'("public"
     "private"
     "protected"
     ";"
     "("
     ")"
     "{"
     "}"
     ",")))
(defun get-tokens () *token-list*)
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

(defun tokenize-c-sharp-fn (string-to-tokenize)
  (let ((rougly-splitted (split #\Space string-to-tokenize :remove-empty-subseqs t))
	(new-list ()))
    (labels ((add-new-token (string-part)  
	       ;;(labels ((push-and-add (string-part )))
	       (multiple-value-bind (start-index last-index) (find-token string-part (get-tokens))
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
