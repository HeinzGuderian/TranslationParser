(in-package :code-generator-utils-space)

(defun export-all-symbols (package-symbol)
    (let ((pack (find-package package-symbol)))
      (do-all-symbols (sym pack) 
	(when (eql (symbol-package sym) pack) (export sym)))))
