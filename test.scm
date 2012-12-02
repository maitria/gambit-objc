(include "objc.scm")

(define-syntax test
  (syntax-rules (test)
    ((test expr)
     (if (not expr)
       (raise 'expr)))))

;; Foreign type for Objective-C objects
(test (not (objc.id? 42)))
(test (objc.id? (objc.objc_getClass "NSObject")))
(test (not (objc.id? (string->selector "copy"))))

;; Foreign type for Objective-C selectors
(test (not (selector? 42)))
(test (selector? (string->selector "stringByAppendingString:")))
(test (not (selector? (objc.objc_getClass "NSObject"))))

(test (string=? (objc.class_getName (objc.objc_getClass "NSString")) "NSString"))

(test (string=? (selector->string (string->selector "stringByAppendingString:")) "stringByAppendingString:"))


