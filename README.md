gambit-objc
-----------

Objective-C bindings for Gambit Scheme.

Installing
----------

Gambit scheme must be installed and in the path.  At that point, it should be
as easy as:

    $ make install

`objc-repl`
-----------

`objc-repl` is a REPL with Objective-C support.  You can use it like so:

    $ objc-repl
    Gambit v4.6.7

    > (import-classes (NSString))
    > (define s [NSString stringWithUTF8String: "Hello, World!"])
    > (define x [NSString stringWithUTF8String: " -- Yay!"])
    > (define y [s stringByAppendingString: x])
    > [y UTF8String]
    "Hello, World! -- Yay!"
    >

Square Braces
-------------

Gambit has the ability to read square braces and insert a symbol.  The
':' symbol is what Objective-C uses to map to Objective-C calls.  The first
two lines of bin/objc-repl.scm demonstrates how to set this up.

The reader must be set up before any source is read.  This might work with
`gsc`'s `-prelude` option.
