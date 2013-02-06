(import x86_64-trampoline)
(export trampoline-allocate)

(define (trampoline-allocate trampoline parameters)
  (let allocate ((remaining-parameters parameters)
                 (next-gp 0)
		 (next-sse 0))
    (if (null? remaining-parameters)
      #!void
      (let word-loop ((parameter (car remaining-parameters))
		      (next-gp next-gp)
		      (next-sse next-sse))
	(if (null? parameter)
	  (allocate (cdr remaining-parameters) next-gp next-sse)
	  (cond
	    ((eq? 'gp (caar parameter))
	     (trampoline-gp-set! trampoline next-gp (cdar parameter))
	     (word-loop (cdr parameter) (+ 1 next-gp) next-sse))
	    ((eq? 'sse (caar parameter))
	     (trampoline-sse-set! trampoline next-sse (cdar parameter))
	     (word-loop (cdr parameter) next-gp (+ 1 next-sse)))
	    (else
	     (raise "don't know how to handle"))))))))

