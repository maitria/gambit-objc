(include "../lib/expect#.scm")
(include "../lib/objc#.scm")

(c-declare #<<EOF
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
EOF
)

(import-classes (NSObject NSString TestMethods))

;; Objects
(expect (not (object? 42)))
(expect (object? NSObject))
(expect (not (object? (string->selector "copy"))))
(expect (equal? #f (class "FlobdarfloobleXXX-")))

;; Selectors
(expect (not (selector? 42)))
(expect (selector? (string->selector "hi mom")))
(expect (not (selector? (class "NSObject"))))

;; Parsing Scheme forms to Objective-C calls
(define-macro (extract-selector-name-from-arg-list arg-list)
  `(quote ,(objc#extract-selector-name-from-arg-list arg-list)))

(expect (equal? "foo" (extract-selector-name-from-arg-list (foo))))
(expect (equal? "forInt:" (extract-selector-name-from-arg-list (forInt: 42))))
(expect (equal? "forInt:orLong:" (extract-selector-name-from-arg-list (forInt: 42 orLong: 99))))

(define-macro (extract-args-from-arg-list arg-list)
  `(quote ,(objc#extract-args-from-arg-list arg-list)))

(expect (equal? '() (extract-args-from-arg-list (foo))))
(expect (equal? '(42) (extract-args-from-arg-list (forInt: 42))))
(expect (equal? '(42 99) (extract-args-from-arg-list (forInt: 42 orLong: 99))))

;; Calling
(define-macro (expect-method method-name #!key to-return)
  `(expect (equal? ,to-return (TestMethods ,method-name))))

(expect-method 'methodReturningYES to-return: #t)
(expect-method 'methodReturningNO to-return: #f)
(expect-method 'methodReturningC99YES to-return: #t)
(expect-method 'methodReturningC99NO to-return: #f)
(expect-method 'voidMethod to-return: #!void)
(expect-method 'onewayVoidMethod to-return: #!void)
(expect-method 'methodReturningInt42 to-return: 42)
(expect-method 'methodReturningLong43 to-return: 43)
(expect-method 'methodReturningShort41 to-return: 41)

(expect-method 'methodReturningUnsignedShort40 to-return: 40)
(expect-method 'methodReturningUnsignedInt39 to-return: 39)
(expect-method 'methodReturningUnsignedLong99 to-return: 99)
(expect-method 'methodReturningFloat2 to-return: 2.0)
(expect-method 'methodReturningDouble2 to-return: 2.0)
(expect-method 'methodReturningLongLong to-return: (arithmetic-shift 1 62))
(expect-method 'methodReturningCString to-return: "a C string")
(expect-method 'methodReturningNil to-return: '())


(expect (selector? (TestMethods 'methodReturningSEL)))
(expect (object? (TestMethods 'methodReturningNSObject)))
(expect (object? (TestMethods 'methodReturningClass)))

(expect "returned objects are callable"
  (string=? "Hello!!" ((NSString stringWithUTF8String: "Hello!!") 'UTF8String)))

(expect (equal? 1142 (TestMethods methodReturningThisInt: 1142)))
(expect (equal? 6642 (TestMethods methodIgnoringThisInt: 1142 andReturningThisOne: 6642)))
(expect (equal? #t (TestMethods methodReturningThisBOOL: #t)))
(expect (equal? #f (TestMethods methodReturningThisC99Bool: #f)))
(expect (equal? 4211 (TestMethods methodReturningThisShort: 4211)))
(expect (equal? 2323 (TestMethods methodReturningThisUnsignedShort: 2323)))
(expect (equal? 9234 (TestMethods methodReturningThisUnsignedInt: 9234)))
(expect (equal? 3492 (TestMethods methodReturningThisLong: 3492)))
(expect (equal? 7272 (TestMethods methodReturningThisUnsignedLong: 7272)))
(expect (equal? 2.0 (TestMethods methodReturningThisFloat: 2.0)))
;(expect (equal? 2.0 (TestMethods methodReturningThisDouble: 2.0)))
(expect (string=? "Hello!!" (TestMethods methodReturningThisCString: "Hello!!")))

(expect "methods can pass through selector arguments"
  (string=? "copy"
	    (selector->string (TestMethods methodReturningThisSEL: (string->selector "copy")))))

(expect "methods can pass through class arguments"
  (string=? ((NSObject 'description) 'UTF8String)
	    (((TestMethods methodReturningThisClass: NSObject) 'description) 'UTF8String)))

(expect "methods can pass through object arguments"
  (string=? "Hello, World!"
	    ((TestMethods methodReturningThisObject: (NSString stringWithUTF8String: "Hello, World!")) 'UTF8String)))

(expect "calling a non-existant method will raise an exception"
  (raises? (lambda () (TestMethods 'methodWhichDoesNotExist))))

;; Expansion of square-brace form
(define-macro (expand-objc-call . args)
  `(quote ,(apply objc-call-expander args)))

(expect "expansion of no-argument method call to work"
  (equal? '(call-method NSObject (string->selector "copy"))
	  (expand-objc-call NSObject copy)))
(expect "expansion of method with arguments to work"
  (equal? '(call-method NSObject (string->selector "forInt:orInt:") 42 79)
	  (expand-objc-call NSObject forInt: 42 orInt: 79)))

(display-expect-results)
