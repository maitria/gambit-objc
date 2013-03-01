(namespace ("expect#"
  raises?
  display-expect-results
  ))

(define-macro (expect #!rest args)
  (if (= 2 (length args))
    (let ((message (car args))
	  (test-expression (cadr args)))
      `(if (not ,test-expression)
	 (raise ,message)))
    `(if (not ,(car args))
       (raise ',(car args)))))
