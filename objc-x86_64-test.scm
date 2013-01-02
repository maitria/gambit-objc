(import expect)
(import objc-x86_64)

;; Sizes of C types
(expect (= 1 (sizeof #\c)))
(expect (= 4 (sizeof #\i)))
(expect (= 2 (sizeof #\s)))
(expect (= 4 (sizeof #\l)))
(expect (= 8 (sizeof #\q)))
(expect (= 1 (sizeof #\C)))
(expect (= 4 (sizeof #\I)))
(expect (= 2 (sizeof #\S)))
(expect (= 4 (sizeof #\L)))
(expect (= 8 (sizeof #\Q)))
(expect (= 4 (sizeof #\f)))
(expect (= 8 (sizeof #\d)))
(expect (= 1 (sizeof #\B)))
(expect (= 8 (sizeof #\*)))
(expect (= 8 (sizeof #\@)))
(expect (= 8 (sizeof #\#)))
(expect (= 8 (sizeof #\:)))
(expect (= 8 (sizeof #\^)))
(expect (= 8 (sizeof #\?)))

;; Classifying C types
(define (expect-each-of type-code-list #!key to-be-classified-as)
  (for-each
    (lambda (type-code)
      (expect 
	(string-append "expected #\\"
		       (list->string (list type-code))
		       " to be classified as "
		       (symbol->string to-be-classified-as))
	(eq? to-be-classified-as (classify type-code))))
    type-code-list))

(define *integral-types* '(#\c #\i #\s #\l #\q #\C #\I #\S #\L #\Q #\B))
(define *pointer-types* '(#\@ #\# #\: #\^ #\?))
(define *floating-point-types* '(#\f #\d))

(expect-each-of *integral-types* to-be-classified-as: 'INTEGER)
(expect-each-of *pointer-types* to-be-classified-as: 'INTEGER)
(expect-each-of *floating-point-types* to-be-classified-as: 'SSE)

(display-expect-results)
