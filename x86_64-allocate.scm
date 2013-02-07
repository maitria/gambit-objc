(import x86_64-trampoline)
(export
  trampoline-allocate
  trampoline-allocate/sse->u64)

(define trampoline-allocate/sse->u64
  (c-lambda (double)
	    unsigned-int64
    "___result = *(unsigned long *)&___arg1;"))

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

  (define (word->u64 word)
    (if (eq? 'gp (car word))
      (cdr word)
      (trampoline-allocate/sse->u64 (cdr word))))

  (define (store-parameter/stack words)
    (set! stack (append (map word->u64 words) stack)))

  (define (store-parameter/registers words)
    (for-each store-word words))

  (define (number-of-words-having-type words type)
    (let ((count 0))
      (for-each
	(lambda (word)
	  (if (eq? type (car word))
	    (set! count (+ 1 count))))
	words)
      count))

  (define (ok-for-registers? words)
    (let ((gp-wanted (number-of-words-having-type words 'gp))
	  (sse-wanted (number-of-words-having-type words 'sse)))
      (and (<= (length words) 2)
	   (<= (+ next-gp gp-wanted) *trampoline-gp-count*)
	   (<= (+ next-sse sse-wanted) *trampoline-sse-count*))))

  (define (store-parameter words)
    (if (ok-for-registers? words)
      (store-parameter/registers words)
      (store-parameter/stack words)))

  (define (write-stack-to-trampoline)
    (trampoline-stack-size-set! trampoline (length stack))
    (let stack-loop ((stack-left stack)
		     (i 0))
      (cond
	((null? stack-left)
	 #!void)
	(else
	 (trampoline-stack-set! trampoline i (car stack-left))
	 (stack-loop (cdr stack-left) (+ i 1))))))

  (for-each store-parameter parameters)
  (write-stack-to-trampoline))

