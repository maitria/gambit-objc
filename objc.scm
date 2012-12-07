(c-define (##instance-tags) () scheme-object "instance_tags" "static"
  '(objc.id))

(c-declare #<<END
#define OBJC2_UNAVAILABLE /* Avoid deprecation warnings */

#include <objc/message.h>
#include <CoreFoundation/CoreFoundation.h>
#include <string.h>
#include <stdlib.h>

static ___SCMOBJ id_to_SCMOBJ(id objc_result, ___SCMOBJ *scm_result, char const* return_type_signature);

#define CALL_FOR_IMP_RESULT(_type) \
  _type imp_result = ((_type (*) (id,SEL,...))imp) (object, sel);
  

static ___SCMOBJ call_method(id object, SEL sel, ___SCMOBJ *result, ___SCMOBJ args)
{
  ___SCMOBJ err = ___NUL;

  Class class = (Class)object_getClass(object);
  Method method = class_getInstanceMethod(class, sel);
  IMP imp = method_getImplementation(method);

  char *return_type_signature = method_copyReturnType(method);
  switch (*return_type_signature) { 
  case 'f':
    {
      CALL_FOR_IMP_RESULT(float)
      err = ___EXT(___FLOAT_to_SCMOBJ) (imp_result, result, -1);
      break;
    }

  case 'd':
    {
      CALL_FOR_IMP_RESULT(double)
      err = ___EXT(___DOUBLE_to_SCMOBJ) (imp_result, result, -1);
      break;
    }

  default:
    {
      CALL_FOR_IMP_RESULT(id)
      err = id_to_SCMOBJ(imp_result, result, return_type_signature);
      break;
    }
  }
  free(return_type_signature);
  return err;
}

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
  INTEGRAL_TYPE('L',ULONG,unsigned long)
  INTEGRAL_TYPE('l',LONG,long)
  INTEGRAL_TYPE('Q',ULONGLONG,unsigned long long)
  INTEGRAL_TYPE('q',LONGLONG,signed long long)
  case 'r':
    if (return_type_signature[1] == '*')
      return ___EXT(___CHARSTRING_to_SCMOBJ) ((char*)objc_result, scm_result, -1);
    break;
  case '@':
    return take_instance(objc_result, scm_result);
  }
  fprintf(stderr, "UNKNOWN RETURN TYPE: %s\n", return_type_signature);
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

