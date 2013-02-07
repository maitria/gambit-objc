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

(expect "TRAMPOLINE-ALLOCTE can allocate SSE registers"
  (let ((t (make-trampoline)))
    (trampoline-allocate t '(((sse . 0.1))
			     ((sse . 0.2))
			     ((sse . 0.4))
			     ((sse . 0.8))
			     ((sse . 1.6))
			     ((sse . 3.2))
			     ((sse . 6.4))
			     ((sse . 12.8))))
    (expect (= 0.1 (trampoline-sse-ref t 0)))
    (expect (= 0.2 (trampoline-sse-ref t 1)))
    (expect (= 0.4 (trampoline-sse-ref t 2)))
    (expect (= 0.8 (trampoline-sse-ref t 3)))
    (expect (= 1.6 (trampoline-sse-ref t 4)))
    (expect (= 3.2 (trampoline-sse-ref t 5)))
    (expect (= 6.4 (trampoline-sse-ref t 6)))
    (expect (= 12.8 (trampoline-sse-ref t 7)))))

(expect "TRAMPOLINE-ALLOCATE can handle interleaved values"
  (let ((t (make-trampoline)))
    (trampoline-allocate t '(((sse . 0.1))
			     ((gp . 11))
			     ((sse . 0.2))
			     ((gp . 22))
			     ((sse . 0.4))
			     ((gp . 33))
			     ((sse . 0.8))
			     ((gp . 44))
			     ((sse . 1.6))
			     ((gp . 55))
			     ((sse . 3.2))
			     ((gp . 66))
			     ((sse . 6.4))
			     ((sse . 12.8))))
    (expect (= 11 (trampoline-gp-ref t 0)))
    (expect (= 22 (trampoline-gp-ref t 1)))
    (expect (= 33 (trampoline-gp-ref t 2)))
    (expect (= 44 (trampoline-gp-ref t 3)))
    (expect (= 55 (trampoline-gp-ref t 4)))
    (expect (= 66 (trampoline-gp-ref t 5)))
    (expect (= 0.1 (trampoline-sse-ref t 0)))
    (expect (= 0.2 (trampoline-sse-ref t 1)))
    (expect (= 0.4 (trampoline-sse-ref t 2)))
    (expect (= 0.8 (trampoline-sse-ref t 3)))
    (expect (= 1.6 (trampoline-sse-ref t 4)))
    (expect (= 3.2 (trampoline-sse-ref t 5)))
    (expect (= 6.4 (trampoline-sse-ref t 6)))
    (expect (= 12.8 (trampoline-sse-ref t 7)))))

(expect "TRAMPOLINE-ALLOCATE will use registers for double qwords"
  (let ((t (make-trampoline)))
    (trampoline-allocate t '(((gp . 11)
			      (gp . 22))
			     ((sse . 1.28)
			      (sse . 12.8))
			     ((gp . 33)
			      (sse . 6.4))
			     ((sse . 3.2)
			      (gp . 44))))
    (expect (= 11 (trampoline-gp-ref t 0)))
    (expect (= 22 (trampoline-gp-ref t 1)))
    (expect (= 33 (trampoline-gp-ref t 2)))
    (expect (= 44 (trampoline-gp-ref t 3)))
    (expect (= 1.28 (trampoline-sse-ref t 0)))
    (expect (= 12.8 (trampoline-sse-ref t 1)))
    (expect (= 6.4 (trampoline-sse-ref t 2)))
    (expect (= 3.2 (trampoline-sse-ref t 3)))))

(expect "TRAMPOLINE-ALLOCATE will use the stack for a large parameter"
  (let ((t (make-trampoline)))
    (trampoline-allocate t '(((gp . 11)
			      (gp . 22)
			      (gp . 33))))
    (expect (= 11 (trampoline-stack-ref t 0)))
    (expect (= 22 (trampoline-stack-ref t 1)))
    (expect (= 33 (trampoline-stack-ref t 2)))
    (expect (= 3 (trampoline-stack-size t)))
    (expect (not (= 11 (trampoline-gp-ref t 0))))
    (expect (not (= 22 (trampoline-gp-ref t 1))))
    (expect (not (= 33 (trampoline-gp-ref t 2))))))

(expect "TRAMPOLINE-ALLOCATE handles multiple large parameters"
  (let ((t (make-trampoline)))
    (trampoline-allocate t '(((gp . 11)
			      (gp . 22)
			      (gp . 33))
			     ((gp . 44)
			      (gp . 55)
			      (gp . 66))))
    (expect (= 44 (trampoline-stack-ref t 0)))
    (expect (= 55 (trampoline-stack-ref t 1)))
    (expect (= 66 (trampoline-stack-ref t 2)))
    (expect (= 11 (trampoline-stack-ref t 3)))
    (expect (= 22 (trampoline-stack-ref t 4)))
    (expect (= 33 (trampoline-stack-ref t 5)))
    (expect (= 6 (trampoline-stack-size t)))))

(expect "TRAMPOLINE-ALLOCATE adds parameters to the stack after gp registers are full"
  (let ((t (make-trampoline)))
    (trampoline-allocate t '(((gp . 11))
			     ((gp . 22))
			     ((gp . 33))
			     ((gp . 44))
			     ((gp . 55))
			     ((gp . 66))
			     ((gp . 77))
			     ((gp . 88))))
    (expect (= 66 (trampoline-gp-ref t 5)))
    (expect (= 77 (trampoline-stack-ref t 1)))
    (expect (= 88 (trampoline-stack-ref t 0)))))

(display-expect-results)
