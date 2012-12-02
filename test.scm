(include "objc.scm")

(define-syntax test
  (syntax-rules (test)
    ((test expr)
     (if (not expr)
       (raise 'expr)))))

;; Foreign type for Objective-C objects
(test (not (objc.id? 42)))
(test (objc.id? (class "NSObject")))
(test (not (objc.id? (string->selector "copy"))))

;; Classes
(test (string=? (class-name (class "NSString")) "NSString"))

;; Selectors
(test (not (selector? 42)))
(test (selector? (string->selector "stringByAppendingString:")))
(test (not (selector? (class "NSObject"))))

(test (string=? (selector->string (string->selector "stringByAppendingString:")) "stringByAppendingString:"))

