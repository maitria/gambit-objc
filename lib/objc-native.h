#include "objc-call.h"
#include "objc-type.h"
#include <string.h>
#include <stdlib.h>

static char *skip_qualifiers(char *signature)
{
	static const char *IGNORABLE_METHOD_QUALIFIERS = "rnNoORV";
	while (*signature && strchr(IGNORABLE_METHOD_QUALIFIERS, *signature))
		++signature;
	return signature;
}

static void adjust_for_alignment(size_t *offset, int alignment)
{
	int alignment_remainder = *offset % alignment;
	if (alignment_remainder)
		*offset += (alignment - *offset);
}

static void delete_struct_type(struct objc_type *struct_type)
{
	int i;
	for (i = 0; struct_type->elements[i]; ++i) {
		struct objc_type *element_type = struct_type->elements[i];
		if (element_type->delete)
			element_type->delete(element_type);
	}

	free(struct_type->call_type->elements);
	free(struct_type->call_type);
	free(struct_type);
}

static ___SCMOBJ return_struct(struct objc_type *type, void *value, ___SCMOBJ *result)
{
	int i, element_count;
	size_t offset = 0;

	for (element_count = 0; type->elements[element_count]; ++element_count)
		;

	*result = ___EXT(___make_vector) (element_count, ___FAL, ___STILL);

	for (i = 0; type->elements[i]; ++i) {
		struct objc_type *element_type = type->elements[i];
		___SCMOBJ error = ___FIX(___NO_ERR), element = ___NUL;
		adjust_for_alignment(&offset, element_type->call_type->alignment);

		error = element_type->convert_return (element_type, ((char *)value) + offset, &element);
		if (error != ___FIX(___NO_ERR))
			return error;

		___VECTORSET((*result), ___FIX(i), element);

		offset += element_type->call_type->size;
	}

	return ___FIX(___NO_ERR);
}

static struct objc_type *parse_type(struct objc_call *call, char **signaturep);

static struct objc_type *parse_struct_type(struct objc_call *call, char **signaturep)
{
	++ *signaturep;

	struct objc_type *struct_type = (struct objc_type*)malloc(sizeof(struct objc_type));
	memset(struct_type, 0, sizeof(struct objc_type));

	struct_type->call_type = (ffi_type *)malloc(sizeof(ffi_type));
	memset(struct_type->call_type, 0, sizeof(ffi_type));
	struct_type->call_type->alignment = 1;
	struct_type->call_type->type = FFI_TYPE_STRUCT;

	struct_type->delete = delete_struct_type;
	struct_type->convert_return = return_struct;

	while (**signaturep != '=')
		++ *signaturep;
	++ *signaturep;

	int element_count = 0;
	struct_type->elements = (struct objc_type**)malloc(sizeof(struct objc_type*)*1);
	struct_type->elements[0] = NULL;
	struct_type->call_type->elements = (ffi_type**)malloc(sizeof(ffi_type*)*1);
	struct_type->call_type->elements[0] = NULL;

	while (**signaturep != '}') {
		struct objc_type *member_type = parse_type(call, signaturep);
		++element_count;

		struct_type->call_type->elements = (ffi_type**)realloc(struct_type->call_type->elements, sizeof(ffi_type*)*(element_count+1));
		// FIXME: Handle OOM
		struct_type->call_type->elements[element_count-1] = member_type->call_type;
		struct_type->call_type->elements[element_count] = NULL;

		struct_type->elements = (struct objc_type**)realloc(struct_type->elements, sizeof(struct objc_type*)*(element_count+1));
		// FIXME: Handle OOM
		struct_type->elements[element_count-1] = member_type;
		struct_type->elements[element_count] = NULL;

		// update size and alignment of structure
		if (member_type->call_type->alignment > struct_type->call_type->alignment)
			struct_type->call_type->alignment = member_type->call_type->alignment;

		adjust_for_alignment(&struct_type->call_type->size, member_type->call_type->alignment);
		struct_type->call_type->size += member_type->call_type->size;
	}
		
	++ *signaturep;
	return struct_type;
}

static struct objc_type *parse_type(struct objc_call *call, char **signaturep)
{
        *signaturep = skip_qualifiers(*signaturep);
        if (**signaturep == '{')
		return parse_struct_type(call, signaturep);
        struct objc_type *type = find_simple_objc_type(**signaturep);
        ++ *signaturep;
        return type;
}

static struct objc_type *call_parameter_type(struct objc_call *call, int n)
{
        char *signature, *scanp;
        
        signature = scanp = method_copyArgumentType(call->method, n);
        if (!signature)
                return NULL;

        struct objc_type *type = parse_type(call, &scanp);
        free(signature);
        return type;
}

static ___SCMOBJ call_find_parameter_types(struct objc_call *call)
{
        int i;
        for (i = 0; i < call->parameter_count; ++i)
                if (!(call->parameter_types[i] = call_parameter_type(call, i)))
                        return ___FIX(___UNKNOWN_ERR);
        return ___FIX(___NO_ERR);
}

static ___SCMOBJ call_parse_parameters(struct objc_call *call, ___SCMOBJ args)
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

		err = call->parameter_types[i]->make_parameter (
			call->parameter_types[i],
			call->parameter_values[i],
			arg
			);
		if (err != ___FIX(___NO_ERR))
			return err;
	}
	return ___FIX(___NO_ERR);
}

static struct objc_type *call_return_type(struct objc_call *call)
{
        char *scanp = (char*)method_getTypeEncoding(call->method);
        return parse_type(call, &scanp);
}

static ___SCMOBJ call_invoke(struct objc_call *call, ___SCMOBJ *result)
{
	ffi_cif cif;
	struct objc_type *return_type = call_return_type(call);
	void *return_value = alloca(return_type->call_type->size);
	ffi_type **arg_types = alloca(sizeof(ffi_type*) * call->parameter_count);
	int i;

	for (i = 0; i < call->parameter_count; ++i)
                arg_types[i] = call->parameter_types[i]->call_type;

	if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, call->parameter_count,
			 return_type->call_type, arg_types) != FFI_OK)
                return ___FIX(___UNKNOWN_ERR);

	ffi_call(&cif, (void (*)())call->imp, return_value, call->parameter_values);

	___SCMOBJ error = return_type->convert_return (return_type, return_value, result);

	if (return_type->delete)
		return_type->delete (return_type);

	return error;
}

static void call_clean_up(struct objc_call *call)
{
	int i;
	for (i = 0; i < call->parameter_count; ++i) {
		if (call->parameter_types[i]->release_parameter)
			call->parameter_types[i]->release_parameter (
				call->parameter_types[i],
				call->parameter_values[i]
				);
		free(call->parameter_values[i]);

		if (call->parameter_types[i]->delete)
			call->parameter_types[i]->delete (call->parameter_types[i]);
	}
}

static ___SCMOBJ call_method(id target, SEL selector, ___SCMOBJ *result, ___SCMOBJ args)
{
	struct objc_call call;
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

        err = call_find_parameter_types(&call);
        if (err != ___FIX(___NO_ERR))
                goto done;

	err = call_parse_parameters(&call, args);
	if (err != ___FIX(___NO_ERR))
                goto done;

	err = call_invoke(&call, result);
done:
	call_clean_up(&call);
	return err;
}

