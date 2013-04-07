;;
;; This is an example of a minimal Cocoa app.  It was transliterated into
;; gambit-objc from C and is based on a blog post found here:
;; 
;;  http://www.cocoawithlove.com/2010/09/minimalist-cocoa-programming.html
;;
(include "../lib/objc#.scm")

(c-declare "#import <Cocoa/Cocoa.h>")

(import-classes (
  NSApplication
  NSAutoreleasePool
  NSMenu
  NSMenuItem
  NSProcessInfo
  NSString
  NSWindow
  ))

(define NSApplicationActivationPolicyRegular
  ((c-lambda ()
	     unsigned-int
     "___result = NSApplicationActivationPolicyRegular;")))

(define NSTitledWindowMask
  ((c-lambda ()
	     unsigned-int
     "___result = NSTitledWindowMask;")))

(define NSBackingStoreBuffered
  ((c-lambda ()
	     unsigned-int
     "___result = NSBackingStoreBuffered;")))

(let* ((NSApp (: NSApplication sharedApplication))
       (menubar (: (: NSMenu new) autorelease))
       (appMenuItem (: (: NSMenuItem new) autorelease))
       (appMenu (: (: NSMenu new) autorelease))
       (appName (: (: NSProcessInfo processInfo) processName))
       (quitTitle (: (: NSString stringWithUTF8String: "Quit ") stringByAppendingString: appName))
       (quitMenuItem (: (: (: NSMenuItem alloc) initWithTitle: quitTitle
						       action: (string->selector "terminate:")
						keyEquivalent: (: NSString stringWithUTF8String: "q")) autorelease))
       (window (: (: (: NSWindow alloc) initWithContentRect: '#( #(0.0 0.0) #(200.0 200.0) )
		                                  styleMask: NSTitledWindowMask
					            backing: NSBackingStoreBuffered
						      defer: #f) autorelease)))
  (: NSApp setActivationPolicy: NSApplicationActivationPolicyRegular)
  (: menubar addItem: appMenuItem)
  (: NSApp setMainMenu: menubar)
  (: appMenu addItem: quitMenuItem)
  (: appMenuItem setSubmenu: appMenu)
  (: window cascadeTopLeftFromPoint: '#(20.0 20.0))
  (: window setTitle: appName)
  (: window makeKeyAndOrderFront: '())
  (: NSApp activateIgnoringOtherApps: #t)
  (: NSApp run))
      
