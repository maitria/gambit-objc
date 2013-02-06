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

  (define (store-parameter words)
    (for-each store-word words))

  (for-each store-parameter parameters))

