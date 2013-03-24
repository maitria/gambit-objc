(include "objc#.scm")

(namespace ("objc#"
  objc.id
  objc.SEL
  make-object-tags
  make-selector-tags
  is_selector
  is_object
  old-wr
  ))

(c-define-type objc.id (pointer (struct "objc_object") (objc.id)))
(c-define-type objc.SEL (pointer (struct "objc_selector") (objc.SEL)))

(c-define (make-object-tags) () scheme-object "object_tags" "___HIDDEN"
  '(objc.id))
(c-define (make-selector-tags) () scheme-object "selector_tags" "___HIDDEN"
  '(objc.SEL))

(define (selector? thing)
  (and (foreign? thing)
       (memq 'objc.SEL (foreign-tags thing))))

(c-define (is_selector thing) (scheme-object) bool "is_selector" "___HIDDEN"
  (objc#selector? thing))

(define (object? thing)
  (and (foreign? thing)
       (memq 'objc.id (foreign-tags thing))))

(c-define (is_object thing) (scheme-object) bool "is_object" "___HIDDEN"
  (objc#object? thing))

(c-declare "#include \"../../lib/objc-native.c\"")

(define class
  (c-lambda (nonnull-char-string)
	    objc.id
    "objc_getClass"))

(define string->selector
  (c-lambda (nonnull-char-string)
	    objc.SEL
    "sel_getUid"))

(define selector->string
  (c-lambda (objc.SEL)
	    char-string
    "___result = (char*)sel_getName(___arg1);"))

(define (call-method object selector . args)
  ((c-lambda (objc.id objc.SEL scheme-object)
	     scheme-object
     "___err = call_method(___arg1, ___arg2, &___result, ___arg3);")
     object selector args))

(let ((old-object-printer ##wr))
  (set! ##wr
    (lambda (we obj)
      (cond
	((selector? obj)
	 (##wr-str we "#<SEL \"")
	 (##wr-str we (selector->string obj))
	 (##wr-str we "\">"))
      (else
	(old-object-printer we obj))))))

