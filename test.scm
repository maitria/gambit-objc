(include "objc.scm")
(include "expect.scm")

;; Objects
(expect (not (object? 42)))
(expect (object? (class "NSObject")))
(expect (not (object? (string->selector "copy"))))

;; Selectors
(expect (not (selector? 42)))
(expect (selector? (string->selector "hi mom")))
(expect (not (selector? (class "NSObject"))))
(expect (string=? (selector->string (string->selector "hi mom")) "hi mom"))

;; Calling
(define TestMethods (class "TestMethods"))

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

(expect (equal? 1142
		(begin
		  (TestMethods methodTakingInt: 1142)
		  (TestMethods 'lastIntPassed))))

(expect (equal? 6642
		(begin
		  (TestMethods methodTakingInt: 1142 andInt: 6642)
		  (TestMethods 'lastIntPassed))))

(expect (equal? "foo" (##extract-selector-name-from-arg-list '(foo))))
(expect (equal? "forInt:" (##extract-selector-name-from-arg-list '(forInt: 42))))
(expect (equal? "forInt:orLong:" (##extract-selector-name-from-arg-list '(forInt: 42 orLong: 99))))

(expect (equal? '() (##extract-args-from-arg-list '(foo))))
(expect (equal? '(42) (##extract-args-from-arg-list '(forInt: 42))))
(expect (equal? '(42 99) (##extract-args-from-arg-list '(forInt: 42 orLong: 99))))

(display-expect-results)
