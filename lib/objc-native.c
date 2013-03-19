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

struct objc_type {
  char objc_name;
  ffi_type *call_type;
  ___SCMOBJ (* parse_return) (void *, ___SCMOBJ *);
};

static ___SCMOBJ parse_id_return(void *value, ___SCMOBJ *result)
{
  return take_object(*(id *)value, result);
}

static ___SCMOBJ parse_SEL_return(void *value, ___SCMOBJ *result)
{
  return ___EXT(___POINTER_to_SCMOBJ) (*(SEL *)value, selector_tags(), NULL, result, -1);
}

static ___SCMOBJ parse_void_return(void *value, ___SCMOBJ *result)
{
  *result = ___VOID;
  return ___FIX(___NO_ERR);
}

static ___SCMOBJ parse_boolean_return(void *value, ___SCMOBJ *result)
{
  *result = *(char *)value ? ___TRU : ___FAL;
  return ___FIX(___NO_ERR);
}

#define RETURN_PARSING_FUNCTION(name,c_type) \
  static ___SCMOBJ parse_##name##_return(void *value, ___SCMOBJ *result) \
  { \
    return ___EXT(___##name##_to_SCMOBJ) (*(c_type *)value, result, -1); \
  }
RETURN_PARSING_FUNCTION(CHARSTRING,char*)
RETURN_PARSING_FUNCTION(FLOAT,float)
RETURN_PARSING_FUNCTION(DOUBLE,double)
RETURN_PARSING_FUNCTION(USHORT,unsigned short)
RETURN_PARSING_FUNCTION(SHORT,signed short)
RETURN_PARSING_FUNCTION(UINT,unsigned int)
RETURN_PARSING_FUNCTION(INT,signed int)
RETURN_PARSING_FUNCTION(ULONG,unsigned long)
RETURN_PARSING_FUNCTION(LONG,long)
RETURN_PARSING_FUNCTION(ULONGLONG,unsigned long long)
RETURN_PARSING_FUNCTION(LONGLONG,signed long long)

struct objc_type OBJC_TYPES[] = {
  { '#', &ffi_type_pointer, parse_id_return },
  { '*', &ffi_type_pointer, parse_CHARSTRING_return },
  { ':', &ffi_type_pointer, parse_SEL_return },
  { '@', &ffi_type_pointer, parse_id_return },
  { 'B', &ffi_type_uint8, parse_boolean_return },
  { 'I', &ffi_type_uint, parse_UINT_return },
  { 'L', &ffi_type_ulong, parse_ULONG_return },
  { 'Q', &ffi_type_uint64, parse_ULONGLONG_return },
  { 'S', &ffi_type_uint16, parse_USHORT_return },
  { 'c', &ffi_type_sint8, parse_boolean_return },
  { 'd', &ffi_type_double, parse_DOUBLE_return },
  { 'f', &ffi_type_float, parse_FLOAT_return },
  { 'i', &ffi_type_sint, parse_INT_return },
  { 'l', &ffi_type_slong, parse_LONG_return },
  { 'q', &ffi_type_sint64, parse_LONGLONG_return },
  { 's', &ffi_type_sint16, parse_SHORT_return },
  { 'v', &ffi_type_void, parse_void_return },
};

static struct objc_type* objc_type_of(char objc_name)
{
  int i = 0;
  for (; i < sizeof(OBJC_TYPES)/sizeof(OBJC_TYPES[0]); ++i)
    if (OBJC_TYPES[i].objc_name == objc_name)
      return &OBJC_TYPES[i];
  assert(0);
  return NULL;
}

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

#define EASY_CONVERSION_CASE(_type,_c_type,_scm_typename,_ffi_typename) \
  case _type: \
    { \
      call->arg_values[call->parameter_count] = malloc(sizeof(_c_type)); \
      if (!call->arg_values[call->parameter_count]) \
        return ___FIX(___UNKNOWN_ERR); \
      err = ___EXT(___SCMOBJ_to_##_scm_typename) ( \
                      arg, \
                      (_c_type *)call->arg_values[call->parameter_count], \
                      -1 \
                      ); \
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

  while (___PAIRP(args)) {
    ___SCMOBJ arg = ___CAR(args);
    ___SCMOBJ err = ___FIX(___NO_ERR);

    call->arg_types[call->parameter_count] = objc_type_of(CALL_next_parameter_type(call))->call_type;

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
        call->arg_values[call->parameter_count] = malloc(sizeof(char*));
        if (!call->arg_values[call->parameter_count])
          return ___FIX(___UNKNOWN_ERR);
        err = ___EXT(___SCMOBJ_to_CHARSTRING) (arg, (char**)call->arg_values[call->parameter_count], -1);
        call->arg_cleaners[call->parameter_count] = ___release_string;
      }
      break;
    case ':':
      {
        if (!is_selector(arg))
          return ___FIX(___UNKNOWN_ERR);
        call->arg_values[call->parameter_count] = malloc(sizeof(SEL));
        SEL sel_arg = ___CAST(SEL, ___CAST(void*,___FIELD(arg,___FOREIGN_PTR)));
        *(SEL*)call->arg_values[call->parameter_count] = sel_arg;
      }
      break;
    case '#':
    case '@':
      {
        if (!is_object(arg))
          return ___FIX(___UNKNOWN_ERR);
        call->arg_values[call->parameter_count] = malloc(sizeof(id));
	err = ___EXT(___SCMOBJ_to_POINTER) (arg, call->arg_values[call->parameter_count], object_tags(), -1);
	if (err != ___FIX(___NO_ERR))
	  return err;
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

static ___SCMOBJ CALL_invoke(CALL *call, ___SCMOBJ *result)
{
  ffi_cif cif;
  char return_value[100];
  struct objc_type *return_type = objc_type_of(CALL_return_type(call));
  call->return_type = return_type->call_type;

  if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, call->parameter_count,
                   call->return_type, call->arg_types) != FFI_OK)
    return ___FIX(___UNKNOWN_ERR);

  ffi_call(&cif, (void (*)())call->imp, return_value, call->arg_values);

  return return_type->parse_return (return_value, result);
}

static void CALL_clean_up(CALL *call)
{
  int i;
  for (i = 0; i < call->parameter_count; ++i) {
    if (call->arg_cleaners[i])
        call->arg_cleaners[i] (*(void**)call->arg_values[i]);
    free(call->arg_values[i]);
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
  if (err != ___FIX(___NO_ERR))
    return err;

  err = CALL_invoke(&call, result);
  CALL_clean_up(&call);
  return err;
}

