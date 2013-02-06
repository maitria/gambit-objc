(import x86_64-trampoline)
(export trampoline-allocate)

(define (trampoline-allocate trampoline parameters)
  (let allocate ((remaining-parameters parameters)
                 (next-gp 0)
		 (next-sse 0))
    (cond
      ((null? remaining-parameters)
       trampoline)
      ((eq? 'gp (caaar remaining-parameters))
       (trampoline-gp-set! trampoline next-gp (cdaar remaining-parameters))
       (allocate (cdr remaining-parameters) (+ 1 next-gp) next-sse))
      ((eq? 'sse (caaar remaining-parameters))
       (trampoline-sse-set! trampoline next-sse (cdaar remaining-parameters))
       (allocate (cdr remaining-parameters) next-gp (+ 1 next-sse)))
      (else
       (raise "don't know how to handle")))))

