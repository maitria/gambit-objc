(import x86_64-trampoline)
(export
  load-trampoline)

(define (load-trampoline #!key address)
  (let ((trampoline (make-trampoline)))
    (trampoline-imp-set! trampoline address)
    trampoline))
