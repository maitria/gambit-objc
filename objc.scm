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
   info.parameter_words[0], info.parameter_words[1], info.parameter_words[2], \
   info.parameter_words[3], info.parameter_words[4], info.parameter_words[5], \
   info.parameter_words[6], info.parameter_words[7], info.parameter_words[8], \
   info.parameter_words[9], info.parameter_words[10], info.parameter_words[11], \
   info.parameter_words[12], info.parameter_words[13], info.parameter_words[14], \
   info.parameter_words[15] \
   )

typedef struct {
  Class class;
  Method method;
  IMP imp;
  int parameter_words[MAX_PARAMETER_WORDS];
} CallInfo;

#define CALL_FOR_IMP_RESULT(_type,_result) \
  _type _result = ((_type (*) (id,SEL,...))info.imp) IMP_PARAMETERS;
#define EASY_CONVERSION_CASE(spec,name,c_type) \
  case spec: \
    { \
      CALL_FOR_IMP_RESULT(c_type,objc_result) \
      return ___EXT(___##name##_to_SCMOBJ) ((c_type) objc_result, result, -1); \
    }
#define IGNORABLE_METHOD_QUALIFIERS \
  "rnNoORV"

static ___SCMOBJ make_parameter_words(int words[MAX_PARAMETER_WORDS], ___SCMOBJ args)
{
  int *current_word = words;
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

static ___SCMOBJ call_method(id object, SEL sel, ___SCMOBJ *result, ___SCMOBJ args)
{
  CallInfo info;

  memset(&info, 0, sizeof(info));
  info.class = (Class)object_getClass(object);
  info.method = class_getInstanceMethod(info.class, sel);
  info.imp = method_getImplementation(info.method);

  ___SCMOBJ err = make_parameter_words(info.parameter_words, args);
  if (err != ___FIX(___NO_ERR)) {
    return err;
  }

  char const *type_signature = skip_qualifiers(method_getTypeEncoding(info.method));
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
      info.imp IMP_PARAMETERS;
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

