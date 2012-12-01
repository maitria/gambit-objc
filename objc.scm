(c-declare "#include <objc/objc-runtime.h>")

(define objc.objc_getClass
  (c-lambda (nonnull-char-string)
	    (type "id" (id))
    "objc_getClass"))

(define objc.class_getName
  (c-lambda ((type "Class" (Class id)))
	    char-string
    "___result = (char*) class_getName(___arg1);"))
