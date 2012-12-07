#import <Foundation/Foundation.h>

@interface TestMethods
@end

@implementation TestMethods

+ (BOOL)methodReturningYES { return YES; }
+ (BOOL)methodReturningNO { return NO; }
+ (void)voidMethod {}
+ (int)methodReturningInt42 { return 42; }
+ (long)methodReturningLong43 { return 43L; }
+ (short)methodReturningShort41 { return 41; }
+ (unsigned short)methodReturningUnsignedShort40 { return 40; }
+ (unsigned int)methodReturningUnsignedInt39 { return 39; }
+ (unsigned long)methodReturningUnsignedLong99 { return 99UL; }
+ (const char *)methodReturningCString { return "a C string"; }
+ (NSObject *)methodReturningNSObject { return [[[NSObject alloc] init] autorelease]; }
+ (NSObject *)methodReturningNil { return nil; }

@end

