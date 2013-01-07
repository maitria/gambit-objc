(import expect)
(import objc-x86_64)

(let* ((result (parse-type "i" 0))
       (next-offset (car result))
       (type (cdr result)))
  (expect (= 1 next-offset))
  (expect (type-signed? type)))

(define *integral-types* '("c" "i" "s" "l" "q" "C" "I" "S" "L" "Q" "B"))
(define *pointer-types* '("@" "#" ":" "^" "?" "*"))
(define *floating-point-types* '("f" "d"))

(define (expect-each-of type-code-list to-have keyword value)
  (define (descriptive-message type-code)
    (string-append "expected #\\"
		   type-code
		   " to have "
		   (keyword->string keyword)
		   " of "
		   (cond
		     ((symbol? value)
		      (symbol->string value))
		     ((number? value)
		      (number->string value)))))

  (for-each
    (lambda (type-code)
      (let ((type (cdr (parse-type type-code 0))))
	(expect
	  (descriptive-message type-code)
	  (equal? value (type-info type keyword)))))
    type-code-list))

;; Sizes of C types
(expect-each-of '("B" "C" "c") to-have: size: 1)
(expect-each-of '("S" "s") to-have: size: 2)
(expect-each-of '("I" "L" "i" "l" "f") to-have: size: 4)
(expect-each-of '("Q" "q" "d") to-have: size: 8)
(expect-each-of *pointer-types* to-have: size: 8)

;; Classifying C types
(expect-each-of *integral-types* to-have: class: 'INTEGER)
(expect-each-of *pointer-types* to-have: class: 'INTEGER)
(expect-each-of *floating-point-types* to-have: class: 'SSE)

(display-expect-results)
