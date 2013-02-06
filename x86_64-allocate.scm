(import x86_64-trampoline)
(export trampoline-allocate)

(define (trampoline-allocate #!key address)
  (let ((trampoline (make-trampoline)))
    (trampoline-imp-set! trampoline address)
    trampoline))
