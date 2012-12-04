#include <string.h>
#include <stdlib.h>

static ___SCMOBJ        call_method(id object, SEL sel, ___SCMOBJ *result, ___SCMOBJ args);
static ___SCMOBJ        id_to_SCMOBJ(id objc_result, ___SCMOBJ *scm_result, char const* return_type_signature);

static ___SCMOBJ call_method(id object, SEL sel, ___SCMOBJ *result, ___SCMOBJ args)
{
        Class class = (Class)object_getClass(object);
        Method method = class_getInstanceMethod(class, sel);
        IMP imp = method_getImplementation(method);
        id objc_result = imp(object, sel);

        char *return_type_signature = method_copyReturnType(method);
        ___SCMOBJ err = id_to_SCMOBJ(objc_result, result, return_type_signature);
        free(return_type_signature);
        return err;
}

static ___SCMOBJ id_to_SCMOBJ(id objc_result, ___SCMOBJ *scm_result, char const* return_type_signature)
{
        switch (*return_type_signature) {
        case 'c':
                *scm_result = objc_result ? ___TRU : ___FAL;
                return ___FIX(___NO_ERR);
        case 'v':
                *scm_result = ___VOID;
                return ___FIX(___NO_ERR);
        case 'i':
                return ___EXT(___INT_to_SCMOBJ) ((int) objc_result, scm_result, -1);
        case '@':
                if ((BOOL)objc_msgSend(objc_result, sel_getUid("isKindOfClass:"), objc_getClass("NSString"))) {
                        ___SCMOBJ str = ___NUL;
                        ___SCMOBJ err = ___FIX(___NO_ERR);
                        char *charp = (char*)objc_msgSend(objc_result, sel_getUid("UTF8String"));
                        err = ___EXT(___CHARSTRING_to_SCMOBJ) (charp, scm_result, -1);
                        if (err != ___FIX(___NO_ERR))
                                return ___FIX(___UNKNOWN_ERR);
                        return ___FIX(___NO_ERR);
                }
        default:
                return ___FIX(___UNIMPL_ERR);
        }
}

