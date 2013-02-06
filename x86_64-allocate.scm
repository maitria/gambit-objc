(import x86_64-trampoline)
(export trampoline-allocate)

(define (trampoline-allocate trampoline parameters)
  (let allocate ((remaining-parameters parameters)
                 (next-gp 0)
		 (next-sse 0))
    (if (null? remaining-parameters)
      trampoline
      (let ((parameter (car remaining-parameters)))
	(cond
	  ((eq? 'gp (caar parameter))
	   (trampoline-gp-set! trampoline next-gp (cdar parameter))
	   (allocate (cdr remaining-parameters) (+ 1 next-gp) next-sse))
	  ((eq? 'sse (caar parameter))
	   (trampoline-sse-set! trampoline next-sse (cdar parameter))
	   (allocate (cdr remaining-parameters) next-gp (+ 1 next-sse)))
	  (else
	   (raise "don't know how to handle")))))))

