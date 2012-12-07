(include "objc.scm")

(define-syntax test
  (syntax-rules (test)
    ((test expr)
     (if (not expr)
       (raise 'expr)))))

;; Instances
(test (not (instance? 42)))
(test (instance? (class "NSObject")))
(test (not (instance? (string->selector "copy"))))

;; Selectors
(test (not (selector? 42)))
(test (selector? (string->selector "stringByAppendingString:")))
(test (not (selector? (class "NSObject"))))
(test (string=? (selector->string (string->selector "stringByAppendingString:")) "stringByAppendingString:"))

;; Calling
(define (test-method-returns return-value method-name)
  (test (equal? return-value (call-method (class "TestMethods") (string->selector method-name)))))

(test-method-returns #t "methodReturningYES")
(test-method-returns #f "methodReturningNO")
(test-method-returns #t "methodReturningC99YES")
(test-method-returns #f "methodReturningC99NO")
(test-method-returns (void) "voidMethod")
(test-method-returns (void) "onewayVoidMethod")
(test-method-returns 42 "methodReturningInt42")
(test-method-returns 43 "methodReturningLong43")
(test-method-returns 41 "methodReturningShort41")
(test-method-returns 40 "methodReturningUnsignedShort40")
(test-method-returns 39 "methodReturningUnsignedInt39")
(test-method-returns 99 "methodReturningUnsignedLong99")
(test-method-returns 2.0 "methodReturningFloat2")
(test-method-returns 2.0 "methodReturningDouble2")
(test-method-returns (arithmetic-shift 1 62) "methodReturningLongLong")
(test-method-returns "a C string" "methodReturningCString")
(test-method-returns '() "methodReturningNil")

(test (instance? (call-method (class "TestMethods") (string->selector "methodReturningNSObject"))))

