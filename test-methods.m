#import <Foundation/Foundation.h>

@interface TestMethods
@end

@implementation TestMethods

+ (BOOL)methodReturningYES { return YES; }
+ (BOOL)methodReturningNO { return NO; }
+ (_Bool)methodReturningC99YES { return YES; }
+ (_Bool)methodReturningC99NO { return NO; }
+ (void)voidMethod {}
+ (int)methodReturningInt42 { return 42; }
+ (long)methodReturningLong43 { return 43L; }
+ (short)methodReturningShort41 { return 41; }
+ (unsigned short)methodReturningUnsignedShort40 { return 40; }
+ (unsigned int)methodReturningUnsignedInt39 { return 39; }
+ (unsigned long)methodReturningUnsignedLong99 { return 99UL; }
+ (float)methodReturningFloat2 { return 2.0f; }
+ (double)methodReturningDouble2 { return 2.0f; }
+ (long long)methodReturningLongLong { return 1LL << 62; }
+ (const char *)methodReturningCString { return "a C string"; }
+ (NSObject *)methodReturningNSObject { return [[[NSObject alloc] init] autorelease]; }
+ (NSObject *)methodReturningNil { return nil; }
+ (oneway void)onewayVoidMethod {}
+ (SEL)methodReturningSEL { return @selector(copy); }
+ (Class)methodReturningClass { return [NSString class]; }

@end

