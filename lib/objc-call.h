#ifndef objc_call_h_INCLUDED
#define objc_call_h_INCLUDED

#define OBJC2_UNAVAILABLE /* Avoid deprecation warnings */
#include <objc/message.h>
#include <CoreFoundation/CoreFoundation.h>

#define MAX_ARGS 16

struct objc_call {
	struct objc_type *parameter_types[MAX_ARGS];
	void *parameter_values[MAX_ARGS];

	id target;
	SEL selector;
	Method method;
	IMP imp;
	int parameter_count;
};

#endif

