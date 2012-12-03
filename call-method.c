
static Class NSString;

static void             call_method_init(void);
static ___SCMOBJ        call_method(id object, SEL sel, ___SCMOBJ args);
static ___SCMOBJ        id_to_SCMOBJ(id result);

static void call_method_init(void)
{
        NSString = (Class)objc_getClass("NSString");
}

static ___SCMOBJ call_method(id object, SEL sel, ___SCMOBJ args)
{
        IMP imp = class_getMethodImplementation(object_getClass(object), sel);
        id result = imp(object, sel);
        return id_to_SCMOBJ(result);
}

static ___SCMOBJ id_to_SCMOBJ(id result)
{
        if ((BOOL)objc_msgSend(result, sel_getUid("isKindOfClass:"), NSString)) {
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
