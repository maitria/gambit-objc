(import expect)
(import x86_64-allocate)
(import x86_64-trampoline)

(expect "TRAMPOLINE-ALLOCATE with no parameters succeeds"
  (not (raises? (lambda () (trampoline-allocate (make-trampoline) '())))))

(expect "TRAMPOLINE-ALLOCATE can allocate general registers"
  (let ((t (make-trampoline)))
    (trampoline-allocate t '(((gp . 11))
			     ((gp . 22))
			     ((gp . 33))
			     ((gp . 44))
			     ((gp . 55))
			     ((gp . 66))))
    (expect (= 11 (trampoline-gp-ref t 0)))
    (expect (= 22 (trampoline-gp-ref t 1)))
    (expect (= 33 (trampoline-gp-ref t 2)))
    (expect (= 44 (trampoline-gp-ref t 3)))
    (expect (= 55 (trampoline-gp-ref t 4)))
    (expect (= 66 (trampoline-gp-ref t 5)))))

(display-expect-results)
