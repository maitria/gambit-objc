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

;; Classes
(test (string=? (class-name (class "NSString")) "NSString"))

;; Selectors
(test (not (selector? 42)))
(test (selector? (string->selector "stringByAppendingString:")))
(test (not (selector? (class "NSObject"))))
(test (string=? (selector->string (string->selector "stringByAppendingString:")) "stringByAppendingString:"))

;; Methods
(test (not (method? 42)))
(test (not (method? (string->selector "copy"))))

(define *stringByAppendingString* (instance-method (class "NSString") (string->selector "stringByAppendingString:")))
(test (method? *stringByAppendingString*))
(let ((selector (string->selector "stringByAppendingString:")))
  (test (equal? selector (method-selector (instance-method (class "NSString") selector)))))

(test (string=? "@" (method-return-signature *stringByAppendingString*)))
(test (string=? "@" (method-argument-signature *stringByAppendingString* 0)))
(test (= 1 (method-argument-count *stringByAppendingString*)))

(test (string=? "an NSString" (call-method (class "TestMethods") (string->selector "methodReturningNSString"))))
(test (eq? #t (call-method (class "TestMethods") (string->selector "methodReturningYES"))))
(test (eq? #f (call-method (class "TestMethods") (string->selector "methodReturningNO"))))
(test (eq? (void) (call-method (class "TestMethods") (string->selector "voidMethod"))))
(test (= 42 (call-method (class "TestMethods") (string->selector "methodReturningInt42"))))
(test (= 43 (call-method (class "TestMethods") (string->selector "methodReturningLong43"))))
