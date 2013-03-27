#include "objc-call.h"
#include "objc-resource.h"

void *allocate_for_call(struct objc_call *call, size_t size)
{
	struct objc_resource *resource = malloc(sizeof (struct objc_resource));
	if (resource == NULL)
		return NULL;

	resource->pointer = malloc(size);
	if (resource->pointer == NULL) {
		free(resource);
		return NULL;
	}

	memset(resource->pointer, 0, size);

	resource->release = free;
	
	resource->next = call->resources;
	call->resources = resource;

	return resource->pointer;
}

void free_resources(struct objc_call *call)
{
	struct objc_resource *this, *next;
	this = call->resources;
	while (this) {
		next = this->next;
		this->release (this->pointer);
		free(this);
		this = next;
	}
}
