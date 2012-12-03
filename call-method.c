#include <string.h>

static void             call_method_init(void);
static ___SCMOBJ        call_method(id object, SEL sel, ___SCMOBJ args);
static ___SCMOBJ        id_to_SCMOBJ(id result, char const* return_type_signature);

static ___SCMOBJ call_method(id object, SEL sel, ___SCMOBJ args)
{
        Class class = (Class)object_getClass(object);
        Method method = class_getInstanceMethod(class, sel);
        IMP imp = method_getImplementation(method);
        id result = imp(object, sel);

        char *return_type_signature = method_copyReturnType(method);
        ___SCMOBJ scm_result = id_to_SCMOBJ(result, return_type_signature);
        free(return_type_signature);
        return scm_result;
}

static ___SCMOBJ id_to_SCMOBJ(id result, char const* return_type_signature)
{
        if (!strcmp(return_type_signature, "c"))
                return result ? ___TRU : ___FAL;

        if ((BOOL)objc_msgSend(result, sel_getUid("isKindOfClass:"), objc_getClass("NSString"))) {
                ___SCMOBJ str = ___NUL;
                ___SCMOBJ err = ___FIX(___NO_ERR);
                char *charp = (char*)objc_msgSend(result, sel_getUid("UTF8String"));
                err = ___EXT(___CHARSTRING_to_SCMOBJ) (charp, &str, -1);
                if (err != ___FIX(___NO_ERR))
                        return ___FIX(___UNKNOWN_ERR);
                return str;
        }

        return ___NUL;
}
