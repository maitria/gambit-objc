#include <CoreFoundation/CoreFoundation.h>
#include "internal.h"
#include "objc-type.h"

static ___SCMOBJ pass_id(struct objc_type *type, void *value, ___SCMOBJ parameter)
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

static ___SCMOBJ return_id(struct objc_type *type, void *value, ___SCMOBJ *result)
{
	id object = *(id*)value;
	if (!object) {
		*result = ___NUL;
		return ___FIX(___NO_ERR);
	}

	CFRetain(object);
	return ___EXT(___POINTER_to_SCMOBJ) (object, object_tags(), release_object, result, -1);
}

static ___SCMOBJ pass_SEL(struct objc_type *type, void *value, ___SCMOBJ parameter)
{
	if (!is_selector(parameter))
		return ___FIX(___UNKNOWN_ERR);
	*(SEL*)value = ___CAST(SEL, ___CAST(void*,___FIELD(parameter,___FOREIGN_PTR)));
	return ___FIX(___NO_ERR);
}

static ___SCMOBJ return_SEL(struct objc_type *type, void *value, ___SCMOBJ *result)
{
	return ___EXT(___POINTER_to_SCMOBJ) (*(SEL *)value, selector_tags(), NULL, result, -1);
}

static ___SCMOBJ return_void(struct objc_type *type, void *value, ___SCMOBJ *result)
{
	*result = ___VOID;
	return ___FIX(___NO_ERR);
}

static ___SCMOBJ return_BOOL(struct objc_type *type, void *value, ___SCMOBJ *result)
{
	*result = *(char *)value ? ___TRU : ___FAL;
	return ___FIX(___NO_ERR);
}

#define MAKE_PARAMETER_FUNCTION(name) \
	static ___SCMOBJ pass_##name(struct objc_type *type, void *value, ___SCMOBJ parameter) \
	{ \
		return ___EXT(___SCMOBJ_to_##name) (parameter, value, -1); \
	}
#define RETURN_PARSING_FUNCTION(name,c_type) \
	static ___SCMOBJ return_##name (struct objc_type *type, void *value, ___SCMOBJ *result) \
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

static void release_CHARSTRING(struct objc_type *type, void *value)
{
	___release_string(*(char **)value);
}

static struct objc_type OBJC_TYPES[] = {
	{ 'v', &ffi_type_void,     0, 0, 0,                                0, return_void },
                                        
	{ 'B', &ffi_type_uint8,    0, 0, pass_BOOL,                        0, return_BOOL },
	{ 'c', &ffi_type_sint8,    0, 0, pass_BOOL,                        0, return_BOOL },
                                        
	{ 's', &ffi_type_sint16,   0, 0, pass_SHORT,                       0, return_SHORT },
	{ 'S', &ffi_type_uint16,   0, 0, pass_USHORT,                      0, return_USHORT },
	{ 'i', &ffi_type_sint,     0, 0, pass_INT,                         0, return_INT },
	{ 'I', &ffi_type_uint,     0, 0, pass_UINT,                        0, return_UINT },
	{ 'l', &ffi_type_slong,    0, 0, pass_LONG,                        0, return_LONG },
	{ 'L', &ffi_type_ulong,    0, 0, pass_ULONG,                       0, return_ULONG },
	{ 'q', &ffi_type_sint64,   0, 0, pass_LONGLONG,                    0, return_LONGLONG },
	{ 'Q', &ffi_type_uint64,   0, 0, pass_ULONGLONG,                   0, return_ULONGLONG },
                                        
	{ 'f', &ffi_type_float,    0, 0, pass_FLOAT,                       0, return_FLOAT },
	{ 'd', &ffi_type_double,   0, 0, pass_DOUBLE,                      0, return_DOUBLE },
                                        
	{ '*', &ffi_type_pointer,  0, 0, pass_CHARSTRING, release_CHARSTRING, return_CHARSTRING },
                                        
	{ ':', &ffi_type_pointer,  0, 0, pass_SEL,                         0, return_SEL },
                                        
	{ '#', &ffi_type_pointer,  0, 0, pass_id,                          0, return_id },
	{ '@', &ffi_type_pointer,  0, 0, pass_id,                          0, return_id },
};

struct objc_type* find_simple_objc_type(char objc_name)
{
	int i = 0;
	for (; i < sizeof(OBJC_TYPES)/sizeof(OBJC_TYPES[0]); ++i)
		if (OBJC_TYPES[i].objc_name == objc_name)
			return &OBJC_TYPES[i];
	assert(0);
	return NULL;
}

