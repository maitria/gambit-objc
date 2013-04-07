#ifndef objc_type_h_INCLUDED
#define objc_type_h_INCLUDED

#include "internal.h"
#include <ffi/ffi.h>

struct objc_call;

struct objc_type {
	char objc_name;
	ffi_type *call_type;
	struct objc_type **elements;
	___SCMOBJ (* make_parameter) (struct objc_type *, void *, ___SCMOBJ);
	void (* release_parameter) (struct objc_type *, void *);
	___SCMOBJ (* convert_return) (struct objc_type *, void *, ___SCMOBJ *);
};

struct objc_type *parse_next_type(struct objc_call *call, char **signaturep);

#endif

