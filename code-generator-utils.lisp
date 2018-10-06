(in-package :code-generator-utils-space)

(defun export-all-symbols (package-symbol)
    (let ((pack (find-package package-symbol)))
      (do-all-symbols (sym pack) 
	(when (eql (symbol-package sym) pack) (export sym)))))

(defun simple-list-identical? (list1 list2 &optional (test-fn #'equal))
  (labels ((recurse-check (list-1 list-2)
	   (if (funcall test-fn (car list-1)
			(car list-2))
	       (if (and (null (cdr list-1))
			(null (cdr list-2)))
		   t
		   (recurse-check (cdr list-1) (cdr list-2)))
	       nil)))
    (if (equal (list-length list1)
	       (list-length list2))
	(recurse-check list1 list2)
	nil)))

(defun dotted-pair? (test-list)
  (let ((sec (cdr test-list)))
    (and (consp test-list)
	 (not (null sec))
	 (null (consp (cdr test-list))))))

(defun make-pairs (in-list)
  (loop with toggle = t for (a b) on in-list while b
     when toggle collect (list a b) do (setf toggle (not toggle))))

(defun strip-string-from-string-list (param-list string-to-strip)
  (delete string-to-strip param-list :test #'string=))

(defun trec (rec &optional (base #'dentity))
  (labels
      ((self (tree)
	 (if (atom tree)
	     (if (functionp base)
		 (funcall base tree)
		 base)
	     (funcall rec tree
		      #'(lambda ()
			  (self (car tree)))
		      #'(lambda ()
			  (if (cdr tree)
			      (self (cdr tree))))))))
    #'self))

(defun trec-nodes (node-fn succesor-fn)
  (labels ((continue-callback (node continue-nodes)
	     #'(lambda () (let ((continues (append (funcall succesor-fn node) continue-nodes)))
			    (node-trav (car continues) (cdr continues)))))
	   (node-trav (node continue-nodes)
	     (if (null node)
		 nil
		 (funcall node-fn node (continue-callback node continue-nodes)))))
    #'(lambda (node) (node-trav node ()))))
		 


;;(let ((x (list 1
;;	       (list 2 (list 3))
;;	       (list 4 (list 5)))))
;;  (funcall (trec-nodes #'(lambda (node continue-fn) (if (eq (car node) 2) node (funcall continue-fn))) #'cdr) x))

;; (let ((x (list 1
;; 	       (list 2 (list 3))
;; 	       (list 4 (list 5)))))
;;   (funcall (trec-nodes #'(lambda (node continue-fn) (cons (car node) (funcall continue-fn))) #'cdr) x))
