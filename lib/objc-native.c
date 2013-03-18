#define OBJC2_UNAVAILABLE /* Avoid deprecation warnings */

#include <objc/message.h>
#include <CoreFoundation/CoreFoundation.h>
#include <string.h>
#include <stdlib.h>
#include <ffi/ffi.h>

static ___SCMOBJ release_object(void *object)
{
  CFRelease(object);
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

#define MAX_ARGS 16

typedef struct {
  ffi_type *arg_types[MAX_ARGS];
  void *arg_values[MAX_ARGS];
  void (*arg_cleaners[MAX_ARGS]) (void *);
  ffi_type *return_type;

  id target;
  SEL selector;
  Class class;
  Method method;
  IMP imp;
  int parameter_count;

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

static char CALL_next_parameter_type(CALL *call)
{
  char *signature = method_copyArgumentType(call->method, call->parameter_count);
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

#define EASY_CONVERSION_CASE(_type,_c_type,_scm_typename,_ffi_typename) \
  case _type: \
    { \
      call->arg_types[call->parameter_count] = &ffi_type_##_ffi_typename; \
      call->arg_values[call->parameter_count] = malloc(sizeof(_c_type)); \
      if (!call->arg_values[call->parameter_count]) \
        return ___FIX(___UNKNOWN_ERR); \
      err = ___EXT(___SCMOBJ_to_##_scm_typename) ( \
                      arg, \
                      (_c_type *)call->arg_values[call->parameter_count], \
                      -1 \
                      ); \
      if (err == ___FIX(___NO_ERR)) \
        CALL_add_parameter_data(call, call->arg_values[call->parameter_count], sizeof(_c_type)); \
    } \
    break;

static ___SCMOBJ CALL_parse_parameters(CALL *call, ___SCMOBJ args)
{
  call->arg_types[0] = &ffi_type_pointer;
  call->arg_values[0] = malloc(sizeof(id));
  *(id*)call->arg_values[0] = call->target;

  call->arg_types[1] = &ffi_type_pointer;
  call->arg_values[1] = malloc(sizeof(SEL));
  *(SEL*)call->arg_values[1] = call->selector;

  call->parameter_count = 2;

  call->current_word = call->parameter_words;
  while (___PAIRP(args)) {
    ___SCMOBJ arg = ___CAR(args);
    ___SCMOBJ err = ___FIX(___NO_ERR);
    switch (CALL_next_parameter_type(call)) {
    EASY_CONVERSION_CASE('B',___BOOL,BOOL,uint8)
    EASY_CONVERSION_CASE('c',___BOOL,BOOL,sint8)
    EASY_CONVERSION_CASE('S',unsigned short,USHORT,uint16)
    EASY_CONVERSION_CASE('s',short,SHORT,sint16)
    EASY_CONVERSION_CASE('I',unsigned int,UINT,uint)
    EASY_CONVERSION_CASE('i',int,INT,sint)
    EASY_CONVERSION_CASE('L',unsigned long,ULONG,ulong)
    EASY_CONVERSION_CASE('l',long,LONG,slong)
    EASY_CONVERSION_CASE('Q',unsigned long long,ULONGLONG,uint64)
    EASY_CONVERSION_CASE('q',long long,LONGLONG,sint64)
    EASY_CONVERSION_CASE('f',float,FLOAT,float)
    EASY_CONVERSION_CASE('d',double,DOUBLE,double)
    case '*':
      {
        call->arg_types[call->parameter_count] = &ffi_type_pointer;
        call->arg_values[call->parameter_count] = malloc(sizeof(char*));
        if (!call->arg_values[call->parameter_count])
          return ___FIX(___UNKNOWN_ERR);
        err = ___EXT(___SCMOBJ_to_CHARSTRING) (arg, (char**)call->arg_values[call->parameter_count], -1);
        CALL_add_clean_up_thunk(call, *(char**)call->arg_values[call->parameter_count], ___release_string);
        if (err == ___FIX(___NO_ERR))
          CALL_add_parameter_data(call, call->arg_values[call->parameter_count], sizeof(char*));
      }
      break;
    case ':':
      {
        if (!is_selector(arg))
          return ___FIX(___UNKNOWN_ERR);
        call->arg_types[call->parameter_count] = &ffi_type_pointer;
        call->arg_values[call->parameter_count] = malloc(sizeof(SEL));
        SEL sel_arg = ___CAST(SEL, ___CAST(void*,___FIELD(arg,___FOREIGN_PTR)));
        CALL_add_parameter_data(call, &sel_arg, sizeof(SEL));
        *(SEL*)call->arg_values[call->parameter_count] = sel_arg;
      }
      break;
    case '#':
    case '@':
      {
        if (!is_object(arg))
          return ___FIX(___UNKNOWN_ERR);
        call->arg_types[call->parameter_count] = &ffi_type_pointer;
        call->arg_values[call->parameter_count] = malloc(sizeof(id));
	err = ___EXT(___SCMOBJ_to_POINTER) (arg, call->arg_values[call->parameter_count], object_tags(), -1);
	if (err != ___FIX(___NO_ERR))
	  return err;
        CALL_add_parameter_data(call, call->arg_values[call->parameter_count], sizeof(id));
      }
      break;
    default:
      fprintf(stderr, "Unhandled parameter type: %c\n", CALL_next_parameter_type(call));
      err = ___FIX(___UNIMPL_ERR);
      break;
    }
    if (err != ___FIX(___NO_ERR)) {
      return err;
    }
    args = ___CDR(args);
    ++call->parameter_count;
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
      c_type objc_result = *(c_type *)return_value; \
      return ___EXT(___##name##_to_SCMOBJ) (objc_result, result, -1); \
    }
static ___SCMOBJ CALL_invoke(CALL *call, ___SCMOBJ *result)
{
  ffi_cif cif;
  char return_value[100];

  switch (CALL_return_type(call)) {
  case 'c':
    call->return_type = &ffi_type_sint8;
    break;
  case 'B':
    call->return_type = &ffi_type_uint8;
    break;
  case 'v':
    call->return_type = &ffi_type_void;
  case ':': case '@': case '#': case '*':
    call->return_type = &ffi_type_pointer;
    break;
  case 'f':
    call->return_type = &ffi_type_float;
    break;
  case 'd':
    call->return_type = &ffi_type_double;
    break;
  case 'S':
    call->return_type = &ffi_type_uint16;
    break;
  case 's':
    call->return_type = &ffi_type_sint16;
    break;
  case 'I':
    call->return_type = &ffi_type_uint;
    break;
  case 'i':
    call->return_type = &ffi_type_sint;
    break;
  case 'L':
    call->return_type = &ffi_type_ulong;
    break;
  case 'l':
    call->return_type = &ffi_type_slong;
    break;
  case 'Q':
    call->return_type = &ffi_type_uint64;
    break;
  case 'q':
    call->return_type = &ffi_type_sint64;
    break;
  default:
    assert(0);
    return ___FIX(___UNIMPL_ERR);
  }

  if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, call->parameter_count,
                   call->return_type, call->arg_types) != FFI_OK)
    return ___FIX(___UNKNOWN_ERR);

  ffi_call(&cif, (void (*)())call->imp, return_value, call->arg_values);

  switch (CALL_return_type(call)) {
  case 'c':
  case 'B':
    {
      *result = return_value[0] ? ___TRU : ___FAL;
      return ___FIX(___NO_ERR);
    }
  case 'v':
    {
      *result = ___VOID;
      return ___FIX(___NO_ERR);
    }
  case ':':
    {
      SEL sel_result = *(SEL *)return_value;
      return ___EXT(___POINTER_to_SCMOBJ) (sel_result, selector_tags(), NULL, result, -1);
    }
  case '@':
  case '#':
    {
      id objc_result = *(id *)return_value;
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
  if (!call.method) {
    return ___FIX(___UNIMPL_ERR);
  }
  call.imp = method_getImplementation(call.method);

  ___SCMOBJ err = CALL_parse_parameters(&call, args);
  if (err != ___FIX(___NO_ERR)) {
    return err;
  }

  err = CALL_invoke(&call, result);
  CALL_clean_up(&call);
  return err;
}

