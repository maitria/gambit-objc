#ifndef objc_call_h_INCLUDED
#define objc_call_h_INCLUDED

#define OBJC2_UNAVAILABLE /* Avoid deprecation warnings */
#include <objc/message.h>
#include <CoreFoundation/CoreFoundation.h>

#define MAX_ARGS 16

struct objc_resource;

struct objc_call {
	int parameter_count;
	struct objc_type *parameter_types[MAX_ARGS];
	void *parameter_values[MAX_ARGS];

        struct objc_resource *resources;

	id target;
	SEL selector;
	Method method;
	IMP imp;
};

#endif

