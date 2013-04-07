#include "objc-call.h"
#include "objc-resource.h"

void *allocate_for_call(struct objc_call *call, size_t size)
{
	struct objc_resource *resource = malloc(sizeof (struct objc_resource));
	if (resource == NULL) {
		// FIXME: longjmp()
		return NULL;
	}

	resource->pointer = malloc(size);
	if (resource->pointer == NULL) {
		free(resource);
		// FIXME: longjmp()
		return NULL;
	}

	memset(resource->pointer, 0, size);

	resource->release = free;
	
	resource->next = call->resources;
	call->resources = resource;

	return resource->pointer;
}

void *resize_call_allocation(struct objc_call *call, void *ptr, size_t new_size)
{
	struct objc_resource *resource = call->resources;
	assert(resource != NULL);
	while (resource->pointer != ptr) {
		resource = resource->next;
		assert(resource != NULL);
	}

	// FIXME: Handle OOM
	resource->pointer = realloc(resource->pointer, new_size);
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
