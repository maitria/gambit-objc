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
+ (int)methodReturningThisInt:(int)i { return i; }
+ (int)methodIgnoringThisInt:(int)a andReturningThisOne:(int)b { return b; }
+ (BOOL)methodReturningThisBOOL:(BOOL)b { return b; }
+ (_Bool)methodReturningThisC99Bool:(_Bool)b { return b; }
+ (short)methodReturningThisShort:(short)s { return s; }
+ (long)methodReturningThisLong:(long)l { return l; }
+ (unsigned short)methodReturningThisUnsignedShort:(unsigned short)us { return us; }
+ (unsigned int)methodReturningThisUnsignedInt:(unsigned int)us { return us; }
+ (unsigned long)methodReturningThisUnsignedLong:(unsigned long)ul { return ul; }
+ (float)methodReturningThisFloat:(float)f { return f; }
+ (double)methodReturningThisDouble:(double)d { return d; }
+ (SEL)methodReturningThisSEL:(SEL)sel { return sel; }
+ (Class)methodReturningThisClass:(Class)c { return c; }
+ (const char*)methodReturningThisCString:(const char*)str { return str; }
+ (id)methodReturningThisObject:(id)o { return o; }

@end

