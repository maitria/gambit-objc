(import x86_64-trampoline)
(export trampoline-allocate)

(define (trampoline-allocate trampoline parameters)
  (define next-gp 0)
  (define next-sse 0)

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
    (trampoline-stack-set-size! trampoline (length words))
    (let loop ((words-left words)
	       (i 0))
      (cond
	((null? words-left)
	 #!void)
	(else
	 (trampoline-stack-set! trampoline i (cdar words-left))
	 (loop (cdr words-left) (+ i 1))))))

  (define (store-parameter words)
    (if (<= (length words) 2)
      (for-each store-word words)
      (store-parameter/stack words)))

  (for-each store-parameter parameters))

