(define-macro (expect #!rest args)
  (if (= 2 (length args))
    (let ((message (car args))
	  (test-expression (cadr args)))
      `(if (not ,test-expression)
	 (raise ,message)))
    `(if (not ,(car args))
       (raise ',(car args)))))

(define (raises? proc)
  (with-exception-catcher
    (lambda (exception) #t)
    (lambda () (proc) #f)))

(define (display-expect-results)
  (display "All passed")
  (newline))
