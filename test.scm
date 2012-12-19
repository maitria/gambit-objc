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
(define (expect-method-returns return-value method-name)
  (expect (equal? return-value (call-method (class "TestMethods") (string->selector method-name)))))

;  (expect (equal? to-return (call-method (class "TestMethods") (string->selector name)))))

(expect-method-returns #t "methodReturningYES")
(expect-method-returns #f "methodReturningNO")
(expect-method-returns #t "methodReturningC99YES")
(expect-method-returns #f "methodReturningC99NO")
(expect-method-returns (void) "voidMethod")
(expect-method-returns (void) "onewayVoidMethod")
(expect-method-returns 42 "methodReturningInt42")
(expect-method-returns 43 "methodReturningLong43")
(expect-method-returns 41 "methodReturningShort41")
(expect-method-returns 40 "methodReturningUnsignedShort40")
(expect-method-returns 39 "methodReturningUnsignedInt39")
(expect-method-returns 99 "methodReturningUnsignedLong99")
(expect-method-returns 2.0 "methodReturningFloat2")
(expect-method-returns 2.0 "methodReturningDouble2")
(expect-method-returns (arithmetic-shift 1 62) "methodReturningLongLong")
(expect-method-returns "a C string" "methodReturningCString")
(expect-method-returns '() "methodReturningNil")

(expect (selector? (call-method (class "TestMethods") (string->selector "methodReturningSEL"))))
(expect (object? (call-method (class "TestMethods") (string->selector "methodReturningNSObject"))))
(expect (object? (call-method (class "TestMethods") (string->selector "methodReturningClass"))))

(define (expect-method-parameter parameter-value method-name . args)
  (apply call-method (class "TestMethods") (string->selector method-name) args)
  (expect (equal? parameter-value
		  (call-method (class "TestMethods") (string->selector "lastIntPassed")))))

(expect-method-parameter 1142 "methodTakingInt:" 1142)
(expect-method-parameter 6642 "methodTakingInt:andInt:" 1142 6642)

(display-expect-results)
