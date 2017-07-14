(in-package :tokenizer)

(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))
(abbrev split cl-utilities:split-sequence )
(abbrev split-if cl-utilities:split-sequence-if )
(abbrev dbind destructuring-bind)

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
  (let ((rougly-splitted (split-if (lambda (x) (or (equal x #\Newline)(equal x #\Space) (equal x #\linefeed) (equal x #\return))) string-to-tokenize :remove-empty-subseqs t))
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

