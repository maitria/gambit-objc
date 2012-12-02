(c-declare "#include <objc/objc-runtime.h>")

(c-define-type objc.id (type "id" (objc.id)))
(c-define-type objc.SEL (type "SEL" (objc.SEL)))

(define (objc.id? c)
  (and (foreign? c)
       (memq 'objc.id (foreign-tags c))))

;; Working with Classes
(define objc.class_getName
  (c-lambda (objc.id)
	    char-string
    "___result = (char*) class_getName((Class) ___arg1);"))

(define class
  (c-lambda (nonnull-char-string)
	    objc.id
    "objc_getClass"))

;; Working with Selectors

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

