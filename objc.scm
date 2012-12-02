(c-declare "#define OBJC2_UNAVAILABLE\n#include <objc/objc-runtime.h>")

(c-define-type objc.id (pointer (struct "objc_object") (objc.id)))
(c-define-type objc.SEL (type "SEL" (objc.SEL)))
(c-define-type objc.Method (pointer (struct "objc_method") (objc.Method)))

;; Instances
(define (instance? c)
  (and (foreign? c)
       (memq 'objc.id (foreign-tags c))))

;; Classes
(define class
  (c-lambda (nonnull-char-string)
	    objc.id
    "objc_getClass"))

(define class-name
  (c-lambda (objc.id)
	    char-string
    "___result = (char*) class_getName((Class) ___arg1);"))

;; Selectors
(define (selector? s)
  (and (foreign? s)
       (memq 'objc.SEL (foreign-tags s))))

(define string->selector
  (c-lambda (nonnull-char-string)
	    objc.SEL
    "sel_getUid"))

(define selector->string
  (c-lambda (objc.SEL)
	    char-string
    "___result = (char*) sel_getName(___arg1);"))

;; Methods
(define (method? m)
  (and (foreign? m)
       (memq 'objc.Method (foreign-tags m))))

(define instance-method
  (c-lambda (objc.id objc.SEL)
	    objc.Method
    "___result = class_getInstanceMethod((Class) ___arg1, ___arg2);"))


