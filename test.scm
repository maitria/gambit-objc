(include "objc.scm")

(define-syntax test
  (syntax-rules (test)
    ((test expr)
     (if (not expr)
       (raise 'expr)))))

;; Foreign type for Objective-C objects
(test (not (objc.id? 42)))
(test (objc.id? (objc.objc_getClass "NSObject")))
(test (not (objc.id? (objc.sel_getUid "copy"))))

;; Foreign type for Objective-C selectors
(test (not (objc.SEL? 42)))
(test (objc.SEL? (objc.sel_getUid "stringByAppendingString:")))
(test (not (objc.SEL? (objc.objc_getClass "NSObject"))))

(test (string=? (objc.class_getName (objc.objc_getClass "NSString")) "NSString"))

(test (string=? (objc.sel_getName (objc.sel_getUid "stringByAppendingString:")) "stringByAppendingString:"))


