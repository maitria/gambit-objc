#import <Foundation/Foundation.h>

@interface TestMethods
@end

@implementation TestMethods

+ (NSString *)methodReturningNSString
{
	return @"an NSString";
}

+ (BOOL)methodReturningYES
{
	return YES;
}

+ (BOOL)methodReturningNO
{
	return NO;
}

+ (void)voidMethod
{
}

+ (int)methodReturningInt42
{
	return 42;
}

@end

