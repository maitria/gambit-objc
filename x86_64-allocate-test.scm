(import expect)
(import x86_64-allocate)
(import x86_64-trampoline)

(expect (not (raises? (lambda () (trampoline-allocate (make-trampoline) '())))))

(display-expect-results)
