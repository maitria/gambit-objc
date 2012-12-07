(c-define (##instance-tags) () scheme-object "instance_tags" "static"
  '(objc.id))

(c-declare #<<END
#define OBJC2_UNAVAILABLE /* Avoid deprecation warnings */

#include <objc/message.h>
#include <CoreFoundation/CoreFoundation.h>
#include <string.h>
#include <stdlib.h>

static ___SCMOBJ release_instance(void *instance)
{
  CFRelease((id)instance);
  return ___NUL;
}

static ___SCMOBJ take_instance(id instance, ___SCMOBJ *scm_result)
{
  if (!instance) {
    *scm_result = ___NUL;
    return ___FIX(___NO_ERR);
  }
    
  CFRetain(instance);
  return ___EXT(___POINTER_to_SCMOBJ) (instance, instance_tags(), release_instance, scm_result, -1);
}

#define IMP_PARAMETERS \
  (object, sel)
#define CALL_FOR_IMP_RESULT(_type,_result) \
  _type _result = ((_type (*) (id,SEL,...))imp) IMP_PARAMETERS;
  

#define INTEGRAL_TYPE(spec,name,c_type) \
  case spec: \
    { \
      CALL_FOR_IMP_RESULT(c_type,objc_result) \
      return ___EXT(___##name##_to_SCMOBJ) ((c_type) objc_result, result, -1); \
    }

static ___SCMOBJ call_method(id object, SEL sel, ___SCMOBJ *result, ___SCMOBJ args)
{
  Class class = (Class)object_getClass(object);
  Method method = class_getInstanceMethod(class, sel);
  IMP imp = method_getImplementation(method);

  char const *type_signature = method_getTypeEncoding(method);
  switch (*type_signature) { 
  case 'c':
    {
      CALL_FOR_IMP_RESULT(char,imp_result)
      *result = imp_result ? ___TRU : ___FAL;
      return ___FIX(___NO_ERR);
    }
  case 'v':
    {
      imp IMP_PARAMETERS;
      *result = ___VOID;
      return ___FIX(___NO_ERR);
    }
  case 'r':
    {
      if (type_signature[1] == '*') {
	CALL_FOR_IMP_RESULT(char*,c_string)
	return ___EXT(___CHARSTRING_to_SCMOBJ) (c_string, result, -1);
      }
      break;
    }
  case '@':
    {
      CALL_FOR_IMP_RESULT(id,objc_result)
      return take_instance(objc_result, result);
    }
  INTEGRAL_TYPE('f',FLOAT,float)
  INTEGRAL_TYPE('d',DOUBLE,double)
  INTEGRAL_TYPE('S',USHORT,unsigned short)
  INTEGRAL_TYPE('s',SHORT,signed short)
  INTEGRAL_TYPE('I',UINT,unsigned int)
  INTEGRAL_TYPE('i',INT,signed int)
  INTEGRAL_TYPE('L',ULONG,unsigned long)
  INTEGRAL_TYPE('l',LONG,long)
  INTEGRAL_TYPE('Q',ULONGLONG,unsigned long long)
  INTEGRAL_TYPE('q',LONGLONG,signed long long)
  }
  fprintf(stderr, "UNKNOWN RETURN TYPE: %s\n", type_signature);
  return ___FIX(___UNIMPL_ERR);
}

END
)

(c-define-type objc.id (pointer (struct "objc_object") (objc.id)))
(c-define-type objc.SEL (pointer (struct "objc_selector") (objc.SEL)))

;; Instances
(define (instance? c)
  (and (foreign? c)
       (memq 'objc.id (foreign-tags c))))

;; Classes
(define class
  (c-lambda (nonnull-char-string)
	    objc.id
    "objc_getClass"))

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

;; Calling
(define (call-method object selector . args)
  ((c-lambda (objc.id objc.SEL scheme-object)
	     scheme-object
     "___err = call_method(___arg1, ___arg2, &___result, ___arg3);")
     object selector args))

