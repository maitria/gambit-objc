(include "expect#.scm")

(define (raises? proc)
  (with-exception-catcher
    (lambda (exception) #t)
    (lambda () (proc) #f)))

(define (display-expect-results)
  (display "All passed")
  (newline))
