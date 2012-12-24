(include "objc#.scm")
(include "expect.scm")

(import-classes (NSObject TestMethods))

;; Objects
(expect (not (object? 42)))
(expect (object? NSObject))
(expect (not (object? (string->selector "copy"))))
(expect (equal? #f (class "FlobdarfloobleXXX-")))

;; Selectors
(expect (not (selector? 42)))
(expect (selector? (string->selector "hi mom")))
(expect (not (selector? (class "NSObject"))))

;; Parsing Scheme forms to Objective-C calls
(expect (equal? "foo" (extract-selector-name-from-arg-list '(foo))))
(expect (equal? "forInt:" (extract-selector-name-from-arg-list '(forInt: 42))))
(expect (equal? "forInt:orLong:" (extract-selector-name-from-arg-list '(forInt: 42 orLong: 99))))

(expect (equal? '() (extract-args-from-arg-list '(foo))))
(expect (equal? '(42) (extract-args-from-arg-list '(forInt: 42))))
(expect (equal? '(42 99) (extract-args-from-arg-list '(forInt: 42 orLong: 99))))

;; Calling
(define-macro (expect-method method-name #!key to-return)
  `(expect (equal? ,to-return (TestMethods ,method-name))))

(expect-method 'methodReturningYES to-return: #t)
(expect-method 'methodReturningNO to-return: #f)
(expect-method 'methodReturningC99YES to-return: #t)
(expect-method 'methodReturningC99NO to-return: #f)
(expect-method 'voidMethod to-return: #!void)
(expect-method 'onewayVoidMethod to-return: #!void)
(expect-method 'methodReturningInt42 to-return: 42)
(expect-method 'methodReturningLong43 to-return: 43)
(expect-method 'methodReturningShort41 to-return: 41)
(expect-method 'methodReturningUnsignedShort40 to-return: 40)
(expect-method 'methodReturningUnsignedInt39 to-return: 39)
(expect-method 'methodReturningUnsignedLong99 to-return: 99)
(expect-method 'methodReturningFloat2 to-return: 2.0)
(expect-method 'methodReturningDouble2 to-return: 2.0)
(expect-method 'methodReturningLongLong to-return: (arithmetic-shift 1 62))
(expect-method 'methodReturningCString to-return: "a C string")
(expect-method 'methodReturningNil to-return: '())

(expect (selector? (TestMethods 'methodReturningSEL)))
(expect (object? (TestMethods 'methodReturningNSObject)))
(expect (object? (TestMethods 'methodReturningClass)))

(expect (equal? 1142 (TestMethods methodReturningThisInt: 1142)))
(expect (equal? 6642 (TestMethods methodIgnoringThisInt: 1142 andReturningThisOne: 6642)))
(expect (equal? #t (TestMethods methodReturningThisBOOL: #t)))
(expect (equal? #f (TestMethods methodReturningThisC99Bool: #f)))
(expect (equal? 4211 (TestMethods methodReturningThisShort: 4211)))
(expect (equal? 2323 (TestMethods methodReturningThisUnsignedShort: 2323)))
(expect (equal? 9234 (TestMethods methodReturningThisUnsignedInt: 9234)))
(expect (equal? 3492 (TestMethods methodReturningThisLong: 3492)))
(expect (equal? 7272 (TestMethods methodReturningThisUnsignedLong: 7272)))

(expect "calling a non-existant method will raise an exception"
  (equal? 'got-it
	  (with-exception-handler
	    (lambda (e) 'got-it)
	    (lambda () (TestMethods 'methodWhichDoesNotExist)))))

(display-expect-results)
