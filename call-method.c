
static Class NSString = (Class)0;
static SEL isKindOfClass_ = (SEL)0;
static SEL UTF8String = (SEL)0;

static ___SCMOBJ call_method(id object, SEL sel, ___SCMOBJ args)
{
        IMP f = class_getMethodImplementation(object_getClass(object), sel);
        id result = f(object, sel);

        if (!isKindOfClass_)
                isKindOfClass_ = sel_getUid("isKindOfClass:");
        if (!NSString)
                NSString = (Class)objc_getClass("NSString");
        if (!UTF8String)
                UTF8String = sel_getUid("UTF8String");
        if ((BOOL)objc_msgSend(result, isKindOfClass_, NSString)) {
                ___SCMOBJ str = ___NUL;
                ___SCMOBJ err = ___FIX(___NO_ERR);
                char *charp = (char*)objc_msgSend(result, UTF8String);
                err = ___EXT(___CHARSTRING_to_SCMOBJ) (charp, &str, -1);
                if (err != ___FIX(___NO_ERR))
                        return ___FIX(___UNKNOWN_ERR);
                return str;
        }

        return ___NUL;
}
