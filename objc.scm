(c-declare "#include <objc/objc-runtime.h>")

(c-define-type objc.id (type "id" (objc.id)))
(c-define-type objc.Class (type "Class" (objc.Class objc.id)))
(c-define-type objc.SEL (type "SEL" (objc.SEL)))

;; Working with Classes
(define objc.class_getName
  (c-lambda (objc.Class)
	    char-string
    "___result = (char*) class_getName(___arg1);"))

(define (objc.Class? c)
  (and (foreign? c)
       (memq 'objc.Class (foreign-tags c))))

;; Obtaining Class Definitions
(define objc.objc_getClass
  (c-lambda (nonnull-char-string)
	    objc.id
    "objc_getClass"))

;; Working with Selectors
(define objc.sel_getUid
  (c-lambda (nonnull-char-string)
	    objc.SEL
    "sel_getUid"))

(define objc.sel_getName
  (c-lambda (objc.SEL)
	    char-string
    "___result = (char*) sel_getName(___arg1);"))

