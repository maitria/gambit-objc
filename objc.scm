(compile-options ld-options: "-framework Foundation")

(c-define-type objc.id (pointer (struct "objc_object") (objc.id)))
(c-define-type objc.SEL (pointer (struct "objc_selector") (objc.SEL)))

(define *object-table* (make-table weak-keys: #t weak-values: #t test: eq?))

(c-define (make-object-tags) () scheme-object "object_tags" "___HIDDEN"
  '(objc.id))
(c-define (make-selector-tags) () scheme-object "selector_tags" "___HIDDEN"
  '(objc.SEL))

(define (selector? thing)
  (and (foreign? thing)
       (memq 'objc.SEL (foreign-tags thing))))

(c-define (is_selector thing) (scheme-object) bool "is_selector" "___HIDDEN"
  (objc#selector? thing))

(define (object? thing)
  (and (procedure? thing)
       (table-ref *object-table* thing #f)))

(c-define (is_object thing) (scheme-object) bool "is_object" "___HIDDEN"
  (objc#object? thing))

(c-define (object->raw-object object) (scheme-object) objc.id "object_to_raw_object" "___HIDDEN"
  (table-ref objc#*object-table* object))

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

typedef long parameter_word_t;

#define MAX_PARAMETER_WORDS 16
#define IMP_PARAMETERS \
  (call->target, call->selector, \
   call->parameter_words[0], call->parameter_words[1], call->parameter_words[2], \
   call->parameter_words[3], call->parameter_words[4], call->parameter_words[5], \
   call->parameter_words[6], call->parameter_words[7], call->parameter_words[8], \
   call->parameter_words[9], call->parameter_words[10], call->parameter_words[11], \
   call->parameter_words[12], call->parameter_words[13], call->parameter_words[14], \
   call->parameter_words[15] \
   )

struct CLEAN_UP_THUNK_tag {
  struct CLEAN_UP_THUNK_tag *next;
  void *data;
  void (* function) (void*);
};

typedef struct CLEAN_UP_THUNK_tag CLEAN_UP_THUNK;

typedef struct {
  id target;
  SEL selector;
  Class class;
  Method method;
  IMP imp;

  parameter_word_t *current_word;
  parameter_word_t parameter_words[MAX_PARAMETER_WORDS];

  CLEAN_UP_THUNK *clean_up_thunks;
} CALL;

static const char *skip_qualifiers(const char *signature)
{
  static const char *IGNORABLE_METHOD_QUALIFIERS = "rnNoORV";
  while (*signature && strchr(IGNORABLE_METHOD_QUALIFIERS, *signature))
    ++signature;
  return signature;
}

static char CALL_parameter_type(CALL *call, int parameter_number)
{
  char *signature = method_copyArgumentType(call->method, 2 + parameter_number);
  if (!signature)
    return '!';
  char result = *skip_qualifiers(signature);
  free(signature);
  return result;
}

static void CALL_add_clean_up_thunk(CALL *call, void *data, void (* function) (void*))
{
  CLEAN_UP_THUNK *thunk = (CLEAN_UP_THUNK *)malloc(sizeof(CLEAN_UP_THUNK));
  thunk->data = data;
  thunk->function = function;
  thunk->next = call->clean_up_thunks;
  call->clean_up_thunks = thunk;
}

static void CALL_add_parameter_data(CALL *call, void* ptr, size_t size)
{
  int i;
  if (size <= sizeof(parameter_word_t)) {
    *call->current_word++ = *(parameter_word_t*)ptr;
  } else {
    for (i = 0; i < size; i += sizeof(parameter_word_t)) {
      *call->current_word++ = ((parameter_word_t*)ptr)[i];
    }
  }
}

#define EASY_CONVERSION_CASE(_type,_c_type,_scm_typename) \
  case _type: \
	{ \
	  _c_type value; \
	  err = ___EXT(___SCMOBJ_to_##_scm_typename) (arg, &value, -1); \
	  if (err == ___FIX(___NO_ERR)) \
	    CALL_add_parameter_data(call, &value, sizeof(_c_type)); \
	} \
	break;

static ___SCMOBJ CALL_parse_parameters(CALL *call, ___SCMOBJ args)
{
  call->current_word = call->parameter_words;
  int parameter_number = 0;
  while (___PAIRP(args)) {
    ___SCMOBJ arg = ___CAR(args);
    ___SCMOBJ err = ___FIX(___NO_ERR);
    switch (CALL_parameter_type(call, parameter_number)) {
    EASY_CONVERSION_CASE('B',___BOOL,BOOL)
    EASY_CONVERSION_CASE('c',___BOOL,BOOL)
    EASY_CONVERSION_CASE('S',unsigned short,USHORT)
    EASY_CONVERSION_CASE('s',short,SHORT)
    EASY_CONVERSION_CASE('I',unsigned int,UINT)
    EASY_CONVERSION_CASE('i',int,INT)
    EASY_CONVERSION_CASE('L',unsigned long,ULONG)
    EASY_CONVERSION_CASE('l',long,LONG)
    EASY_CONVERSION_CASE('Q',unsigned long long,ULONGLONG)
    EASY_CONVERSION_CASE('q',long long,LONGLONG)
    EASY_CONVERSION_CASE('f',float,FLOAT)
    EASY_CONVERSION_CASE('d',double,DOUBLE)
    case '*':
      {
	  char *value;
	  err = ___EXT(___SCMOBJ_to_CHARSTRING) (arg, &value, -1);
	  CALL_add_clean_up_thunk(call, value, ___release_string);
	  if (err == ___FIX(___NO_ERR)) \
	    CALL_add_parameter_data(call, &value, sizeof(char*));
      }
      break;
    case ':':
      {
	if (!is_selector(arg))
	  return ___FIX(___UNKNOWN_ERR);
	SEL sel_arg = ___CAST(SEL, ___CAST(void*,___FIELD(arg,___FOREIGN_PTR)));
	CALL_add_parameter_data(call, &sel_arg, sizeof(SEL));
      }
      break;
    case '#':
    case '@':
      {
	if (!is_object(arg))
	  return ___FIX(___UNKNOWN_ERR);
	id id_arg = object_to_raw_object(arg);
	CALL_add_parameter_data(call, &id_arg, sizeof(id));
      }
      break;
    default:
      fprintf(stderr, "Unhandled parameter type: %c\n", CALL_parameter_type(call, parameter_number));
      err = ___FIX(___UNIMPL_ERR);
      break;
    }
    if (err != ___FIX(___NO_ERR)) {
      return err;
    }
    args = ___CDR(args);
    ++parameter_number;
  }
  return ___FIX(___NO_ERR);
}
#undef EASY_CONVERSION_CASE

static char CALL_return_type(CALL *call)
{
  return *skip_qualifiers(method_getTypeEncoding(call->method));
}

#define CALL_FOR_IMP_RESULT(_type,_result) \
  _type _result = ((_type (*) (id,SEL,...))call->imp) IMP_PARAMETERS;
#define EASY_CONVERSION_CASE(spec,name,c_type) \
  case spec: \
    { \
      CALL_FOR_IMP_RESULT(c_type,objc_result) \
      return ___EXT(___##name##_to_SCMOBJ) (objc_result, result, -1); \
    }
static ___SCMOBJ CALL_invoke(CALL *call, ___SCMOBJ *result)
{
  switch (CALL_return_type(call)) { 
  case 'c':
  case 'B':
    {
      CALL_FOR_IMP_RESULT(BOOL,imp_result)
      *result = imp_result ? ___TRU : ___FAL;
      return ___FIX(___NO_ERR);
    }
  case 'v':
    {
      call->imp IMP_PARAMETERS;
      *result = ___VOID;
      return ___FIX(___NO_ERR);
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
  EASY_CONVERSION_CASE('*',CHARSTRING,char*)
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
  fprintf(stderr, "UNKNOWN RETURN TYPE: %c\n", CALL_return_type(call));
  return ___FIX(___UNIMPL_ERR);
}
#undef EASY_CONVERSION_CASE

static void CALL_clean_up(CALL *call)
{
  CLEAN_UP_THUNK *thunk = call->clean_up_thunks, *next;
  while (thunk) {
    thunk->function(thunk->data);
    next = thunk->next;
    free(thunk);
    thunk = next;
  }
}

static ___SCMOBJ call_method(id target, SEL selector, ___SCMOBJ *result, ___SCMOBJ args)
{
  CALL call;

  memset(&call, 0, sizeof(call));
  call.target = target;
  call.selector = selector;
  call.class = (Class)object_getClass(call.target);
  call.method = class_getInstanceMethod(call.class, call.selector);
  if (!call.method)
    return ___FIX(___UNIMPL_ERR);
  call.imp = method_getImplementation(call.method);

  ___SCMOBJ err = CALL_parse_parameters(&call, args);
  if (err != ___FIX(___NO_ERR)) {
    return err;
  }

  err = CALL_invoke(&call, result);
  CALL_clean_up(&call);
  return err;
}

END
)

(define (raw-object? thing)
  (and (foreign? thing)
       (memq 'objc.id (foreign-tags thing))))

(define raw-class
  (c-lambda (nonnull-char-string)
	    objc.id
    "objc_getClass"))

(define (extract-selector-name-from-arg-list args)
  (if (= 1 (length args))
    (symbol->string (car args))
    (let arg-loop ((name-so-far "")
		   (rest-of-args args))
      (if (null? rest-of-args)
	 name-so-far
	 (arg-loop (string-append name-so-far (keyword->string (car rest-of-args)) ":")
		   (cddr rest-of-args))))))

(define (extract-args-from-arg-list arg-list)
  (if (= 1 (length arg-list))
    '()
    (let arg-loop ((reversed-args '())
		   (remaining-arg-list arg-list))
      (if (null? remaining-arg-list)
	(reverse reversed-args)
	(arg-loop (cons (cadr remaining-arg-list) reversed-args)
		  (cddr remaining-arg-list))))))

(define (raw-object->object raw-object)
  (define (object-closure #!rest arg-list)
    (let* ((selector-name (extract-selector-name-from-arg-list arg-list))
	   (args          (extract-args-from-arg-list arg-list))
	   (result	  (apply call-method raw-object (string->selector selector-name) args)))
      (if (raw-object? result)
	(raw-object->object result)
	result)))
  (table-set! *object-table* object-closure raw-object)
  object-closure)

(define (class name)
  (let ((raw-class (raw-class name)))
    (if raw-class
      (raw-object->object raw-class)
      #f)))

(define string->selector
  (c-lambda (nonnull-char-string)
	    objc.SEL
    "sel_getUid"))

(define selector->string
  (c-lambda (objc.SEL)
	    char-string
    "___result = (char*)sel_getName(___arg1);"))

(define (call-method object selector . args)
  ((c-lambda (objc.id objc.SEL scheme-object)
	     scheme-object
     "___err = call_method(___arg1, ___arg2, &___result, ___arg3);")
     object selector args))

(define-macro (import-classes class-list)
  (let class-loop ((resulting-syntax '(begin))
		   (rest-of-classes class-list))
    (if (null? rest-of-classes)
      (reverse resulting-syntax)
      (let ((class-name (car rest-of-classes)))
	(class-loop
	  (cons `(define ,class-name (class ,(symbol->string class-name))) resulting-syntax)
	  (cdr rest-of-classes))))))
