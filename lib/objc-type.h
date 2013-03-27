#ifndef objc_type_h_INCLUDED
#define objc_type_h_INCLUDED

#include "internal.h"
#include <ffi/ffi.h>

struct objc_type {
	char objc_name;
	ffi_type *call_type;
	struct objc_type **elements;
	void (* delete) (struct objc_type *);
	___SCMOBJ (* make_parameter) (struct objc_type *, void *, ___SCMOBJ);
	void (* release_parameter) (struct objc_type *, void *);
	___SCMOBJ (* convert_return) (struct objc_type *, void *, ___SCMOBJ *);
};

struct objc_type* find_simple_objc_type(char objc_name);

#endif

