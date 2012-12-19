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
(define @TestMethods (class "TestMethods"))

(define-macro (expect-method method-name #!key to-return)
  `(expect (equal? ,to-return (call-method @TestMethods (string->selector ,method-name)))))


(expect-method "methodReturningYES" to-return: #t)
(expect-method "methodReturningNO" to-return: #f)
(expect-method "methodReturningC99YES" to-return: #t)
(expect-method "methodReturningC99NO" to-return: #f)
(expect-method "voidMethod" to-return: #!void)
(expect-method "onewayVoidMethod" to-return: #!void)
(expect-method "methodReturningInt42" to-return: 42)
(expect-method "methodReturningLong43" to-return: 43)
(expect-method "methodReturningShort41" to-return: 41)
(expect-method "methodReturningUnsignedShort40" to-return: 40)
(expect-method "methodReturningUnsignedInt39" to-return: 39)
(expect-method "methodReturningUnsignedLong99" to-return: 99)
(expect-method "methodReturningFloat2" to-return: 2.0)
(expect-method "methodReturningDouble2" to-return: 2.0)
(expect-method "methodReturningLongLong" to-return: (arithmetic-shift 1 62))
(expect-method "methodReturningCString" to-return: "a C string")
(expect-method "methodReturningNil" to-return: '())

(expect (selector? (call-method @TestMethods (string->selector "methodReturningSEL"))))
(expect (object? (call-method @TestMethods (string->selector "methodReturningNSObject"))))
(expect (object? (call-method @TestMethods (string->selector "methodReturningClass"))))

(define (expect-method-parameter parameter-value method-name . args)
  (apply call-method @TestMethods (string->selector method-name) args)
  (expect (equal? parameter-value
		  (call-method @TestMethods (string->selector "lastIntPassed")))))

(expect-method-parameter 1142 "methodTakingInt:" 1142)
(expect-method-parameter 6642 "methodTakingInt:andInt:" 1142 6642)

(display-expect-results)
