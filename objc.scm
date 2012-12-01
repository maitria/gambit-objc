(c-declare "#include <objc/objc-runtime.h>")

;; Working with Classes
(define objc.class_getName
  (c-lambda ((type "Class" (Class id)))
	    char-string
    "___result = (char*) class_getName(___arg1);"))

;; Obtaining Class Definitions
(define objc.objc_getClass
  (c-lambda (nonnull-char-string)
	    (type "id" (id))
    "objc_getClass"))

;; Working with Selectors
(define objc.sel_getUid
  (c-lambda (nonnull-char-string)
	    (type "SEL" (SEL))
    "sel_getUid"))

(define objc.sel_getName
  (c-lambda ((type "SEL" (SEL)))
	    char-string
    "___result = (char*) sel_getName(___arg1);"))

