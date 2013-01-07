(import expect)
(import objc-x86_64)

(define *integral-types* '("c" "i" "s" "l" "q" "C" "I" "S" "L" "Q" "B"))
(define *pointer-types* '("@" "#" ":" "^" "?" "*"))
(define *floating-point-types* '("f" "d"))

(define (expect-each-of type-code-list #!key to-be-classified-as to-have-size)
  (define (descriptive-message type-code clause thing)
    (string-append "expected #\\"
		   (list->string (list type-code))
		   clause
		   (cond
		     ((symbol? thing)
		      (symbol->string thing))
		     ((number? thing)
		      (number->string thing)))))

  (for-each
    (lambda (type-code)
      (cond
	(to-be-classified-as
	  (expect 
	    (descriptive-message type-code " to be classified as " to-be-classified-as)
	    (eq? to-be-classified-as (classify type-code))))
	(to-have-size
	  (expect
	    (descriptive-message type-code " to have size " to-have-size)
	    (= to-have-size (sizeof type-code))))))
    type-code-list))

;; Sizes of C types
(expect-each-of '("B" "C" "c") to-have-size: 1)
(expect-each-of '("S" "s") to-have-size: 2)
(expect-each-of '("I" "L" "i" "l" "f") to-have-size: 4)
(expect-each-of '("Q" "q" "d") to-have-size: 8)
(expect-each-of *pointer-types* to-have-size: 8)

;; Classifying C types
(expect-each-of *integral-types* to-be-classified-as: 'INTEGER)
(expect-each-of *pointer-types* to-be-classified-as: 'INTEGER)
(expect-each-of *floating-point-types* to-be-classified-as: 'SSE)

(display-expect-results)
