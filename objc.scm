(c-declare #<<END
#define OBJC2_UNAVAILABLE /* Avoid deprecation warnings */
#include <objc/message.h>

#include "call-method.c"
END
)

(c-define-type objc.id (pointer (struct "objc_object") (objc.id)))
(c-define-type objc.SEL (pointer (struct "objc_selector") (objc.SEL)))
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

(define method-selector
  (c-lambda (objc.Method)
	    objc.SEL
    "method_getName"))

(define method-return-signature
  (c-lambda (objc.Method)
	    char-string
#<<EOF
  char* return_type_buffer = method_copyReturnType(___arg1);
  ___result = return_type_buffer;
#define ___AT_END free(return_type_buffer);
EOF
))

(define method-argument-count
  (c-lambda (objc.Method)
	    unsigned-int
    "___result = method_getNumberOfArguments(___arg1) - 2;"))

(define method-argument-signature
  (c-lambda (objc.Method unsigned-int)
	    char-string
#<<EOF
  char* argument_type_buffer = method_copyArgumentType(___arg1, ___arg2);
  ___result = argument_type_buffer;
#define ___AT_END free(argument_type_buffer);
EOF
))

(define (call-method object selector . args)
  ((c-lambda (objc.id objc.SEL scheme-object)
	     scheme-object
#<<EOF
  ___err = call_method(___arg1, ___arg2, &___result, ___arg3);
EOF
) object selector args))

