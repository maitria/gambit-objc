(import x86_64-trampoline)
(export trampoline-allocate)

(define (trampoline-allocate trampoline parameters)
  (let allocate ((remaining-parameters parameters)
                 (next-gp 0))
    (cond
      ((null? remaining-parameters)
       trampoline)
      (else
       (trampoline-gp-set! trampoline next-gp (cdaar remaining-parameters))
       (allocate (cdr remaining-parameters) (+ 1 next-gp))))))

