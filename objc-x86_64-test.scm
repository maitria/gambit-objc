(import expect)
(import objc-x86_64)

(define *integral-types* '("c" "i" "s" "l" "q" "C" "I" "S" "L" "Q" "B"))
(define *pointer-types* '("@" "#" ":" "^" "?" "*"))
(define *floating-point-types* '("f" "d"))

(define (expect-type type-code to-have keyword value)
  (let ((descriptive-message (string-append
			       "expected '" type-code "'"
			       " to have " (keyword->string keyword)
			       " of " (object->string value)))
	 (type (parse-type type-code)))
    (expect descriptive-message
      (equal? value (type-info type keyword)))))

(define (expect-each-of type-code-list to-have keyword value)
  (for-each
    (lambda (type-code)
      (expect-type type-code to-have keyword value))
    type-code-list))

;; Parsing
(expect "PARSE-TYPE to ignore method qualifiers"
  (equal? (cdr (parse-type "i"))
	  (cdr (parse-type "rnNoORVi"))))

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

;; Signedness of integral types
(expect-each-of '("c" "i" "s" "l" "q") to-have: signed: #t)
(expect-each-of '("C" "I" "S" "L" "Q") to-have: signed: #f)

;; Structs
(expect-type "{foo}" to-have: c-type: "struct foo")
(expect-type "{foo=ii}" to-have: c-type: "struct foo")
(expect "correct advancement past struct specificaton for a simple struct"
  (equal? '(#\x) (car (parse-type/internal (string->list "{foo}x")))))
(expect "correct advancement past struct specificaton for a defined struct"
  (equal? '(#\x) (car (parse-type/internal (string->list "{foo=}x")))))
(expect "correct advancement past nested structs"
  (equal? '(#\x) (car (parse-type/internal (string->list "{foo=i{bar=ii}d}x")))))
(expect "PARSE-TYPE provides #f for members for struct when they aren't specified"
  (eq? #f (cadr (memq members: (parse-type "{foo}")))))
(expect "PARSE-TYPE provides a list of members when they are specified"
  (list? (cadr (memq members: (parse-type "{foo=}")))))
(expect "PARSE-TYPE has the right number of struct members"
  (= 2 (length (cadr (memq members: (parse-type "{foo=id}"))))))

;; Unions
(expect-type "(foo)" to-have: c-type: "union foo")
(expect "correct advancement past nested unions"
  (equal? '(#\x) (car (parse-type/internal (string->list "(foo=i(bar=ii)d)x")))))

(display-expect-results)
