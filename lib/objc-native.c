#define OBJC2_UNAVAILABLE /* Avoid deprecation warnings */
#include <objc/message.h>
#include <CoreFoundation/CoreFoundation.h>
#include <string.h>
#include <stdlib.h>
#include <ffi/ffi.h>

struct objc_type {
	char objc_name;
	ffi_type *call_type;
	___SCMOBJ (* make_parameter) (void *, ___SCMOBJ);
	void (* release_parameter) (void *);
	___SCMOBJ (* convert_return) (void *, ___SCMOBJ *);
};

static ___SCMOBJ pass_id(void *value, ___SCMOBJ parameter)
{
	if (!is_object(parameter))
		return ___FIX(___UNKNOWN_ERR);
	return ___EXT(___SCMOBJ_to_POINTER) (parameter, value, object_tags(), -1);
}

static ___SCMOBJ release_object(void *object)
{
	CFRelease(object);
	return ___NUL;
}

static ___SCMOBJ return_id(void *value, ___SCMOBJ *result)
{
	id object = *(id*)value;
	if (!object) {
		*result = ___NUL;
		return ___FIX(___NO_ERR);
	}

	CFRetain(object);
	return ___EXT(___POINTER_to_SCMOBJ) (object, object_tags(), release_object, result, -1);
}

static ___SCMOBJ pass_SEL(void *value, ___SCMOBJ parameter)
{
	if (!is_selector(parameter))
		return ___FIX(___UNKNOWN_ERR);
	*(SEL*)value = ___CAST(SEL, ___CAST(void*,___FIELD(parameter,___FOREIGN_PTR)));
	return ___FIX(___NO_ERR);
}

static ___SCMOBJ return_SEL(void *value, ___SCMOBJ *result)
{
	return ___EXT(___POINTER_to_SCMOBJ) (*(SEL *)value, selector_tags(), NULL, result, -1);
}

static ___SCMOBJ return_void(void *value, ___SCMOBJ *result)
{
	*result = ___VOID;
	return ___FIX(___NO_ERR);
}

static ___SCMOBJ return_BOOL(void *value, ___SCMOBJ *result)
{
	*result = *(char *)value ? ___TRU : ___FAL;
	return ___FIX(___NO_ERR);
}

#define MAKE_PARAMETER_FUNCTION(name) \
	static ___SCMOBJ pass_##name(void *value, ___SCMOBJ parameter) \
	{ \
		return ___EXT(___SCMOBJ_to_##name) (parameter, value, -1); \
	}
#define RETURN_PARSING_FUNCTION(name,c_type) \
	static ___SCMOBJ return_##name (void *value, ___SCMOBJ *result) \
	{ \
		return ___EXT(___##name##_to_SCMOBJ) (*(c_type *)value, result, -1); \
	}
MAKE_PARAMETER_FUNCTION(BOOL)
MAKE_PARAMETER_FUNCTION(FLOAT)
RETURN_PARSING_FUNCTION(FLOAT,float)
MAKE_PARAMETER_FUNCTION(DOUBLE)
RETURN_PARSING_FUNCTION(DOUBLE,double)
MAKE_PARAMETER_FUNCTION(USHORT)
RETURN_PARSING_FUNCTION(USHORT,unsigned short)
MAKE_PARAMETER_FUNCTION(SHORT)
RETURN_PARSING_FUNCTION(SHORT,signed short)
MAKE_PARAMETER_FUNCTION(UINT)
RETURN_PARSING_FUNCTION(UINT,unsigned int)
MAKE_PARAMETER_FUNCTION(INT)
RETURN_PARSING_FUNCTION(INT,signed int)
MAKE_PARAMETER_FUNCTION(ULONG)
RETURN_PARSING_FUNCTION(ULONG,unsigned long)
MAKE_PARAMETER_FUNCTION(LONG)
RETURN_PARSING_FUNCTION(LONG,long)
MAKE_PARAMETER_FUNCTION(ULONGLONG)
RETURN_PARSING_FUNCTION(ULONGLONG,unsigned long long)
MAKE_PARAMETER_FUNCTION(LONGLONG)
RETURN_PARSING_FUNCTION(LONGLONG,signed long long)

MAKE_PARAMETER_FUNCTION(CHARSTRING)
RETURN_PARSING_FUNCTION(CHARSTRING,char*)

static void release_CHARSTRING(void *value)
{
	___release_string(*(char **)value);
}

static struct objc_type OBJC_TYPES[] = {
	{ 'v', &ffi_type_void,    0,                                0, return_void },

	{ 'B', &ffi_type_uint8,   pass_BOOL,                        0, return_BOOL },
	{ 'c', &ffi_type_sint8,   pass_BOOL,                        0, return_BOOL },

	{ 's', &ffi_type_sint16,  pass_SHORT,                       0, return_SHORT },
	{ 'S', &ffi_type_uint16,  pass_USHORT,                      0, return_USHORT },
	{ 'i', &ffi_type_sint,    pass_INT,                         0, return_INT },
	{ 'I', &ffi_type_uint,    pass_UINT,                        0, return_UINT },
	{ 'l', &ffi_type_slong,   pass_LONG,                        0, return_LONG },
	{ 'L', &ffi_type_ulong,   pass_ULONG,                       0, return_ULONG },
	{ 'q', &ffi_type_sint64,  pass_LONGLONG,                    0, return_LONGLONG },
	{ 'Q', &ffi_type_uint64,  pass_ULONGLONG,                   0, return_ULONGLONG },

	{ 'f', &ffi_type_float,   pass_FLOAT,                       0, return_FLOAT },
	{ 'd', &ffi_type_double,  pass_DOUBLE,                      0, return_DOUBLE },

	{ '*', &ffi_type_pointer, pass_CHARSTRING, release_CHARSTRING, return_CHARSTRING },

	{ ':', &ffi_type_pointer, pass_SEL,                         0, return_SEL },

	{ '#', &ffi_type_pointer, pass_id,                          0, return_id },
	{ '@', &ffi_type_pointer, pass_id,                          0, return_id },
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
	struct objc_type *parameter_types[MAX_ARGS];
	void *parameter_values[MAX_ARGS];

	id target;
	SEL selector;
	Method method;
	IMP imp;
	int parameter_count;
} CALL;

static char *skip_qualifiers(char *signature)
{
	static const char *IGNORABLE_METHOD_QUALIFIERS = "rnNoORV";
	while (*signature && strchr(IGNORABLE_METHOD_QUALIFIERS, *signature))
		++signature;
	return signature;
}

static struct objc_type *parse_type(char **signaturep)
{
        *signaturep = skip_qualifiers(*signaturep);
        return objc_type_of(**signaturep);
}

static struct objc_type *CALL_parameter_type(CALL *call, int n)
{
        char *signature, *scanp;
        
        signature = scanp = method_copyArgumentType(call->method, n);
        if (!signature)
                return NULL;

        struct objc_type *type = parse_type(&scanp);
        free(signature);
        return type;
}

static ___SCMOBJ CALL_find_parameter_types(CALL *call)
{
        int i;
        for (i = 0; i < call->parameter_count; ++i)
                if (!(call->parameter_types[i] = CALL_parameter_type(call, i)))
                        return ___FIX(___UNKNOWN_ERR);
        return ___FIX(___NO_ERR);
}

static ___SCMOBJ CALL_parse_parameters(CALL *call, ___SCMOBJ args)
{
	call->parameter_values[0] = malloc(sizeof(id));
	*(id*)call->parameter_values[0] = call->target;

	call->parameter_values[1] = malloc(sizeof(SEL));
	*(SEL*)call->parameter_values[1] = call->selector;

        int i;
        for (i = 2; ___PAIRP(args); args = ___CDR(args), ++i) {
		___SCMOBJ arg = ___CAR(args);
		___SCMOBJ err = ___FIX(___NO_ERR);

		call->parameter_values[i] = malloc(call->parameter_types[i]->call_type->size);
		if (!call->parameter_values[i])
			return ___FIX(___UNKNOWN_ERR);

		err = call->parameter_types[i]->make_parameter (call->parameter_values[i], arg);
		if (err != ___FIX(___NO_ERR))
			return err;
	}
	return ___FIX(___NO_ERR);
}

static char CALL_return_type(CALL *call)
{
	return *skip_qualifiers((char*)method_getTypeEncoding(call->method));
}

static ___SCMOBJ CALL_invoke(CALL *call, ___SCMOBJ *result)
{
	ffi_cif cif;
	struct objc_type *return_type = objc_type_of(CALL_return_type(call));
	void *return_value = alloca(return_type->call_type->size);
	ffi_type **arg_types = alloca(sizeof(ffi_type*) * call->parameter_count);
	int i;

	for (i = 0; i < call->parameter_count; ++i)
                arg_types[i] = call->parameter_types[i]->call_type;

	if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, call->parameter_count,
			 return_type->call_type, arg_types) != FFI_OK)
                return ___FIX(___UNKNOWN_ERR);

	ffi_call(&cif, (void (*)())call->imp, return_value, call->parameter_values);

	return return_type->convert_return (return_value, result);
}

static void CALL_clean_up(CALL *call)
{
	int i;
	for (i = 0; i < call->parameter_count; ++i) {
		if (call->parameter_types[i]->release_parameter)
			call->parameter_types[i]->release_parameter (call->parameter_values[i]);
		free(call->parameter_values[i]);
	}
}

static ___SCMOBJ call_method(id target, SEL selector, ___SCMOBJ *result, ___SCMOBJ args)
{
	CALL call;
	Class class;
        ___SCMOBJ err = ___FIX(___NO_ERR);

	memset(&call, 0, sizeof(call));
	call.target = target;
	call.selector = selector;
	class = (Class)object_getClass(call.target);
	call.method = class_getInstanceMethod(class, call.selector);
	if (!call.method)
		return ___FIX(___UNIMPL_ERR);
	call.imp = method_getImplementation(call.method);
        call.parameter_count = method_getNumberOfArguments(call.method);

        err = CALL_find_parameter_types(&call);
        if (err != ___FIX(___NO_ERR))
                goto done;

	err = CALL_parse_parameters(&call, args);
	if (err != ___FIX(___NO_ERR))
                goto done;

	err = CALL_invoke(&call, result);
done:
	CALL_clean_up(&call);
	return err;
}

