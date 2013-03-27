#ifndef internal_h_INCLUDED
#define internal_h_INCLUDED

#ifndef ___MODULE_NAME
#include <gambit.h>
#endif

extern int is_selector(___SCMOBJ obj);
extern int is_object(___SCMOBJ objc);
extern ___SCMOBJ selector_tags();
extern ___SCMOBJ object_tags();

#endif

