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
  (call.target, call.selector, \
   call.parameter_words[0], call.parameter_words[1], call.parameter_words[2], \
   call.parameter_words[3], call.parameter_words[4], call.parameter_words[5], \
   call.parameter_words[6], call.parameter_words[7], call.parameter_words[8], \
   call.parameter_words[9], call.parameter_words[10], call.parameter_words[11], \
   call.parameter_words[12], call.parameter_words[13], call.parameter_words[14], \
   call.parameter_words[15] \
   )

typedef struct {
  id target;
  SEL selector;
  Class class;
  Method method;
  IMP imp;
  int parameter_words[MAX_PARAMETER_WORDS];
} CALL;

#define CALL_FOR_IMP_RESULT(_type,_result) \
  _type _result = ((_type (*) (id,SEL,...))call.imp) IMP_PARAMETERS;
#define EASY_CONVERSION_CASE(spec,name,c_type) \
  case spec: \
    { \
      CALL_FOR_IMP_RESULT(c_type,objc_result) \
      return ___EXT(___##name##_to_SCMOBJ) ((c_type) objc_result, result, -1); \
    }
#define IGNORABLE_METHOD_QUALIFIERS \
  "rnNoORV"

static ___SCMOBJ CALL_parse_parameters(CALL *call, ___SCMOBJ args)
{
  int *current_word = call->parameter_words;
  while (___PAIRP(args)) {
    ___SCMOBJ arg = ___CAR(args);
    ___SCMOBJ err = ___EXT(___SCMOBJ_to_INT) (arg, current_word++, -1);
    if (err != ___FIX(___NO_ERR)) {
      return err;
    }
    args = ___CDR(args);
  }
  return ___FIX(___NO_ERR);
}

static const char *skip_qualifiers(const char *signature)
{
  while (*signature && strchr(IGNORABLE_METHOD_QUALIFIERS, *signature))
    ++signature;
  return signature;
}

static char CALL_return_type(CALL *call)
{
  return *skip_qualifiers(method_getTypeEncoding(call->method));
}

static ___SCMOBJ call_method(id target, SEL selector, ___SCMOBJ *result, ___SCMOBJ args)
{
  CALL call;

  memset(&call, 0, sizeof(call));
  call.target = target;
  call.selector = selector;
  call.class = (Class)object_getClass(call.target);
  call.method = class_getInstanceMethod(call.class, call.selector);
  call.imp = method_getImplementation(call.method);

  ___SCMOBJ err = CALL_parse_parameters(&call, args);
  if (err != ___FIX(___NO_ERR)) {
    return err;
  }

  switch (CALL_return_type(&call)) { 
  case 'c':
  case 'B':
    {
      CALL_FOR_IMP_RESULT(BOOL,imp_result)
      *result = imp_result ? ___TRU : ___FAL;
      return ___FIX(___NO_ERR);
    }
  case 'v':
    {
      call.imp IMP_PARAMETERS;
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
  fprintf(stderr, "UNKNOWN RETURN TYPE: %c\n", CALL_return_type(&call));
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

