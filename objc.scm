(c-define (##object-tags) () scheme-object "object_tags" "___HIDDEN"
  '(objc.id))
(c-define (##selector-tags) () scheme-object "selector_tags" "___HIDDEN"
  '(objc.SEL))

(c-declare #<<END
#define OBJC2_UNAVAILABLE /* Avoid deprecation warnings */

#include <objc/message.h>
#include <CoreFoundation/CoreFoundation.h>
#include <string.h>
#include <stdlib.h>

static ___SCMOBJ release_object(void *object)
{
  CFRelease((id)object);
  return ___NUL;
}

static ___SCMOBJ take_object(id object, ___SCMOBJ *scm_result)
{
  if (!object) {
    *scm_result = ___NUL;
    return ___FIX(___NO_ERR);
  }
    
  CFRetain(object);
  return ___EXT(___POINTER_to_SCMOBJ) (object, object_tags(), release_object, scm_result, -1);
}

#define MAX_PARAMETER_WORDS 16
#define IMP_PARAMETERS \
  (object, sel, \
   p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7], \
   p[8], p[9], p[10], p[11], p[12], p[13], p[14], p[15] \
   )
#define CALL_FOR_IMP_RESULT(_type,_result) \
  _type _result = ((_type (*) (id,SEL,...))imp) IMP_PARAMETERS;
#define EASY_CONVERSION_CASE(spec,name,c_type) \
  case spec: \
    { \
      CALL_FOR_IMP_RESULT(c_type,objc_result) \
      return ___EXT(___##name##_to_SCMOBJ) ((c_type) objc_result, result, -1); \
    }
#define IGNORABLE_METHOD_QUALIFIERS \
  "rnNoORV"

static ___SCMOBJ call_method(id object, SEL sel, ___SCMOBJ *result, ___SCMOBJ args)
{
  Class class = (Class)object_getClass(object);
  Method method = class_getInstanceMethod(class, sel);
  IMP imp = method_getImplementation(method);

  int p[MAX_PARAMETER_WORDS] = {};
  int *argp = p;
  if (___PAIRP(args)) {
    ___SCMOBJ arg1 = ___CAR(args);
    ___SCMOBJ err = ___EXT(___SCMOBJ_to_INT) (arg1, argp, -1);
    if (err != ___FIX(___NO_ERR)) {
      return err;
    }
  }

  char const *type_signature = method_getTypeEncoding(method);
  while (strchr(IGNORABLE_METHOD_QUALIFIERS, *type_signature))
    ++type_signature;

  switch (*type_signature) { 
  case 'c':
  case 'B':
    {
      CALL_FOR_IMP_RESULT(BOOL,imp_result)
      *result = imp_result ? ___TRU : ___FAL;
      return ___FIX(___NO_ERR);
    }
  case 'v':
    {
      imp IMP_PARAMETERS;
      *result = ___VOID;
      return ___FIX(___NO_ERR);
    }
  case '*':
    {
      CALL_FOR_IMP_RESULT(char*,c_string)
      return ___EXT(___CHARSTRING_to_SCMOBJ) (c_string, result, -1);
    }
  case ':':
    {
      CALL_FOR_IMP_RESULT(SEL,sel_result)
      return ___EXT(___POINTER_to_SCMOBJ) (sel_result, selector_tags(), NULL, result, -1);
    }
  case '@':
  case '#':
    {
      CALL_FOR_IMP_RESULT(id,objc_result)
      return take_object(objc_result, result);
    }
  EASY_CONVERSION_CASE('f',FLOAT,float)
  EASY_CONVERSION_CASE('d',DOUBLE,double)
  EASY_CONVERSION_CASE('S',USHORT,unsigned short)
  EASY_CONVERSION_CASE('s',SHORT,signed short)
  EASY_CONVERSION_CASE('I',UINT,unsigned int)
  EASY_CONVERSION_CASE('i',INT,signed int)
  EASY_CONVERSION_CASE('L',ULONG,unsigned long)
  EASY_CONVERSION_CASE('l',LONG,long)
  EASY_CONVERSION_CASE('Q',ULONGLONG,unsigned long long)
  EASY_CONVERSION_CASE('q',LONGLONG,signed long long)
  }
  fprintf(stderr, "UNKNOWN RETURN TYPE: %s\n", type_signature);
  return ___FIX(___UNIMPL_ERR);
}

END
)

(c-define-type objc.id (pointer (struct "objc_object") (objc.id)))
(c-define-type objc.SEL (pointer (struct "objc_selector") (objc.SEL)))

(define (object? thing)
  (and (foreign? thing)
       (memq 'objc.id (foreign-tags thing))))

(define class
  (c-lambda (nonnull-char-string)
	    objc.id
    "objc_getClass"))

(define (selector? thing)
  (and (foreign? thing)
       (memq 'objc.SEL (foreign-tags thing))))

(define string->selector
  (c-lambda (nonnull-char-string)
	    objc.SEL
    "sel_getUid"))

(define selector->string
  (c-lambda (objc.SEL)
	    char-string
    "___result = (char*) sel_getName(___arg1);"))

(define (call-method object selector . args)
  ((c-lambda (objc.id objc.SEL scheme-object)
	     scheme-object
     "___err = call_method(___arg1, ___arg2, &___result, ___arg3);")
     object selector args))

