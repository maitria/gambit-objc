(include "objc.scm")

(define-syntax test
  (syntax-rules (test)
    ((test expr)
     (if (not expr)
       (raise 'expr)))))

(test (string=? (objc.class_getName (objc.objc_getClass "NSString")) "NSString"))
(test (string=? (objc.sel_getName (objc.sel_getUid "stringByAppendingString:")) "stringByAppendingString:"))
