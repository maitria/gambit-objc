#ifndef objc_resource_h_INCLUDED
#define objc_resource_h_INCLUDED

#include <string.h>

struct objc_call;

struct objc_resource {
    struct objc_resource *next;
    void *pointer;
    void (* release) (void *);
};

void *allocate_for_call(struct objc_call *call, size_t size);

void free_resources(struct objc_call *call);

#endif // ndef objc_resource_h_INCLUDED

