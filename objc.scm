(c-declare #<<END
#define OBJC2_UNAVAILABLE /* Avoid deprecation warnings */

#include <objc/message.h>
#include <string.h>
#include <stdlib.h>

static ___SCMOBJ        call_method(id object, SEL sel, ___SCMOBJ *result, ___SCMOBJ args);
static ___SCMOBJ        id_to_SCMOBJ(id objc_result, ___SCMOBJ *scm_result, char const* return_type_signature);

static ___SCMOBJ call_method(id object, SEL sel, ___SCMOBJ *result, ___SCMOBJ args)
{
  Class class = (Class)object_getClass(object);
  Method method = class_getInstanceMethod(class, sel);
  IMP imp = method_getImplementation(method);
  id objc_result = imp(object, sel);

  char *return_type_signature = method_copyReturnType(method);
  ___SCMOBJ err = id_to_SCMOBJ(objc_result, result, return_type_signature);
  free(return_type_signature);
  return err;
}

#define INTEGRAL_TYPE(spec,name,c_type) case spec: return ___EXT(___##name##_to_SCMOBJ) ((c_type) objc_result, scm_result, -1);

static ___SCMOBJ id_to_SCMOBJ(id objc_result, ___SCMOBJ *scm_result, char const* return_type_signature)
{
  switch (*return_type_signature) {
  case 'c':
    *scm_result = objc_result ? ___TRU : ___FAL;
    return ___FIX(___NO_ERR);
  case 'v':
    *scm_result = ___VOID;
    return ___FIX(___NO_ERR);
  INTEGRAL_TYPE('S',USHORT,unsigned short)
  INTEGRAL_TYPE('s',SHORT,signed short)
  INTEGRAL_TYPE('I',UINT,unsigned int)
  INTEGRAL_TYPE('i',INT,signed int)
  INTEGRAL_TYPE('Q',ULONG,unsigned long)
  INTEGRAL_TYPE('q',LONG,signed long)
  case '@':
    if ((BOOL)objc_msgSend(objc_result, sel_getUid("isKindOfClass:"), objc_getClass("NSString"))) {
      ___SCMOBJ str = ___NUL;
      ___SCMOBJ err = ___FIX(___NO_ERR);
      char *charp = (char*)objc_msgSend(objc_result, sel_getUid("UTF8String"));
      err = ___EXT(___CHARSTRING_to_SCMOBJ) (charp, scm_result, -1);
      if (err != ___FIX(___NO_ERR))
	return ___FIX(___UNKNOWN_ERR);
      return ___FIX(___NO_ERR);
    }
    if ((BOOL)objc_msgSend(objc_result, sel_getUid("isKindOfClass:"), objc_getClass("NSNumber"))) {
      long longValue = (long)objc_msgSend(objc_result, sel_getUid("longValue"));
      return ___EXT(___LONG_to_SCMOBJ) (longValue, scm_result, -1);
    }
  default:
    fprintf(stderr, "UNKNOWN RETURN TYPE: %s\n", return_type_signature);
    return ___FIX(___UNIMPL_ERR);
  }
}

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

