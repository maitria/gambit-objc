
(define-syntax expect
  (syntax-rules (expect)
    ((expect expr)
     (if (not expr)
       (raise 'expr)))))

(define (display-expect-results)
  (display "All passed")
  (newline))

