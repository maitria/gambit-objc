(include "../lib/expect#.scm")
(include "../lib/x86_64-type#.scm")

(define *integral-types* '("c" "i" "s" "l" "q" "C" "I" "S" "L" "Q" "B"))
(define *pointer-types* '("@" "#" ":" "^" "?" "*"))

(define (expect-type type-code to-have getter-for-value value)
  (let ((descriptive-message (string-append
			       "expected '" type-code "'"
			       " to have " (object->string getter-for-value)
			       " of " (object->string value)))
	 (type (parse-type type-code)))
    (expect descriptive-message
      (equal? value (getter-for-value type)))))

(define (expect-each-of type-code-list to-have getter-for-value value)
  (for-each
    (lambda (type-code)
      (expect-type type-code to-have getter-for-value value))
    type-code-list))

;; Parsing
(expect "PARSE-TYPE to ignore method qualifiers"
  (equal? (parse-type "i"))
	  (parse-type "rnNoORVi"))

;; Sizes of C types
(expect-each-of '("B" "C" "c") to-have: type-size 1)
(expect-each-of '("S" "s") to-have: type-size 2)
(expect-each-of '("I" "L" "i" "l" "f") to-have: type-size 4)
(expect-each-of '("Q" "q" "d") to-have: type-size 8)
(expect-each-of *pointer-types* to-have: type-size 8)

;; Signedness of integral types
(expect-each-of '("c" "i" "s" "l" "q") to-have: type-signed? #t)
(expect-each-of '("C" "I" "S" "L" "Q") to-have: type-signed? #f)

;; Structs
(expect-type "{foo}" to-have: type-c-name "struct foo")
(expect-type "{foo=ii}" to-have: type-c-name "struct foo")
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

;; Unions
(expect-type "(foo)" to-have: type-c-name "union foo")
(expect "correct advancement past nested unions"
  (equal? '(#\x) (car (parse-type/internal (string->list "(foo=i(bar=ii)d)x")))))
(expect "finds greatest element size for union size"
  (= 4 (type-size (parse-type "(foo=icci)"))))

;; PARSE-FUNCTION-SIGNATURE
(expect (equal? (list (parse-type "(foo=icci)")
		      (parse-type "i")
		      (parse-type "Q"))
	        (parse-function-signature "(foo=icci)12i16Q42")))

(display-expect-results)
