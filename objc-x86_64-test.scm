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
  (equal? (parse-type "i"))
	  (parse-type "rnNoORVi"))

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
(expect-type "{foo}" to-have: c-name: "struct foo")
(expect-type "{foo=ii}" to-have: c-name: "struct foo")
(expect "correct advancement past struct specificaton for a simple struct"
  (equal? '(#\x) (car (parse-type/internal (string->list "{foo}x")))))
(expect "correct advancement past struct specificaton for a defined struct"
  (equal? '(#\x) (car (parse-type/internal (string->list "{foo=}x")))))
(expect "correct advancement past nested structs"
  (equal? '(#\x) (car (parse-type/internal (string->list "{foo=i{bar=ii}d}x")))))
(expect "PARSE-TYPE provides #f for members for struct when they aren't specified"
  (eq? #f (type-members (parse-type "{foo}"))))
(expect "PARSE-TYPE provides a list of members when they are specified"
  (list? (type-members (parse-type "{foo=}"))))
(expect "PARSE-TYPE has the right number of struct members"
  (= 2 (length (type-members (parse-type "{foo=id}")))))

(expect "sums sizes for structure size"
  (= 8 (type-size (parse-type "{foo=ii}"))))
(expect "adds alignment padding for structure members"
  (= 12 (type-size (parse-type "{foo=ici}"))))
(expect "adds alignment padding for structure members"
  (= 12 (type-size (parse-type "{foo=icci}"))))

(expect "the struct alginment to be the LCM of members' alignments"
  (= 4 (type-alignment (parse-type "{foo=ici}"))))

(expect "structures larger than two eightbytes are classified as MEMORY"
  (eq? 'MEMORY (type-class (parse-type "{foo=**c}"))))

;; Unions
(expect-type "(foo)" to-have: c-name: "union foo")
(expect "correct advancement past nested unions"
  (equal? '(#\x) (car (parse-type/internal (string->list "(foo=i(bar=ii)d)x")))))
(expect "finds greatest element size for union size"
  (= 4 (type-size (parse-type "(foo=icci)"))))

;; Reducing classifications
(expect "reducing equal classes results in the specified class"
  (eq? 'FOO (reduce-classification/internal 'FOO 'FOO)))
(expect "reducing a NO_CLASS uses the other class"
  (and (eq? 'FOO (reduce-classification/internal 'NO_CLASS 'FOO))
       (eq? 'FOO (reduce-classification/internal 'FOO 'NO_CLASS))))
(expect "reducing a MEMORY makes the result MEMORY"
  (and (eq? 'MEMORY (reduce-classification/internal 'MEMORY 'FOO))
       (eq? 'MEMORY (reduce-classification/internal 'FOO 'MEMORY))))
(expect "reducing an INTEGER makes the result INTEGER"
  (and (eq? 'INTEGER (reduce-classification/internal 'INTEGER 'FOO))
       (eq? 'INTEGER (reduce-classification/internal 'FOO 'INTEGER))))
(expect "catch-all is SSE"
  (eq? 'SSE (reduce-classification/internal 'SSE 'FOO)))

;; Trampoline
(let ((t (make-trampoline)))
  (trampoline-gp-set! t 0 78)
  (expect (= 78 (trampoline-gp-ref t 0))))

(let ((t (make-trampoline)))
  (trampoline-sse-set! t 2 2.0)
  (expect (= 2.0 (trampoline-sse-ref t 2))))

(display-expect-results)
