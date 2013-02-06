(import x86_64-trampoline)
(export trampoline-allocate)

(define (trampoline-allocate trampoline parameters)
  (define next-gp 0)
  (define next-sse 0)

  (define (store-gp value)
    (trampoline-gp-set! trampoline next-gp value)
    (set! next-gp (+ 1 next-gp)))

  (define (store-sse value)
    (trampoline-sse-set! trampoline next-sse value)
    (set! next-sse (+ 1 next-sse)))

  (let allocate ((remaining-parameters parameters))
    (if (null? remaining-parameters)
      #!void
      (let word-loop ((parameter (car remaining-parameters)))
	(if (null? parameter)
	  (allocate (cdr remaining-parameters))
	  (cond
	    ((eq? 'gp (caar parameter))
	     (store-gp (cdar parameter))
	     (word-loop (cdr parameter)))

	    ((eq? 'sse (caar parameter))
	     (store-sse (cdar parameter))
	     (word-loop (cdr parameter)))

	    (else
	     (raise "don't know how to handle"))))))))

