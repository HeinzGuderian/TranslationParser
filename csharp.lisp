;; Goal state
;; Add function to class
;; (fnPattern (public void setClassName) ( (int id) (string name) ) ((settAtt ,name "id.ToString();")) )
;; (onClass testClass settName)

(defmacro fnPattern ((c-visibility c-ret-type c-name) fnParams &body body)
  `(let ((c-name ,c-name))
  (format nil " ~a ( ~a ) { ~a } " 
	   ,(format nil " ~a ~a ~a " c-visibility c-ret-type c-name )
	   ,(format nil "~{~{~a ~}~^, ~}" fnParams ) ;;  '((int id)(string name))
	   ,@body
	   )))
