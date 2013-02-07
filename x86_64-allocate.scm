(import x86_64-trampoline)
(export trampoline-allocate)

(define (trampoline-allocate trampoline parameters)
  (define next-gp 0)
  (define next-sse 0)
  (define stack '())

  (define (store-word/gp value)
    (trampoline-gp-set! trampoline next-gp value)
    (set! next-gp (+ 1 next-gp)))

  (define (store-word/sse value)
    (trampoline-sse-set! trampoline next-sse value)
    (set! next-sse (+ 1 next-sse)))

  (define (store-word word)
    (if (eq? 'gp (car word))
      (store-word/gp (cdr word))
      (store-word/sse (cdr word))))

  (define (store-parameter/stack words)
    (set! stack (append words stack)))

  (define (store-parameter words)
    (if (<= (length words) 2)
      (for-each store-word words)
      (store-parameter/stack words)))

  (for-each store-parameter parameters)

  (trampoline-stack-set-size! trampoline (length stack))
  (let stack-loop ((stack-left stack)
		   (i 0))
    (cond
      ((null? stack-left)
       #!void)
      (else
       (trampoline-stack-set! trampoline i (cdar stack-left))
       (stack-loop (cdr stack-left) (+ i 1))))))

