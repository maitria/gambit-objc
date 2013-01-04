(export
  classify
  sizeof)

(define *type-info*
  '(
    ;; Boolean
    (#\B c-type: "_Bool" size: 1 alignment: 1 class: INTEGER)

    ;; Integral types
    (#\c c-type: "char" size: 1 alignment: 1 class: INTEGER)
    (#\C c-type: "unsigned char" size: 1 alignment: 1 class: INTEGER)
    (#\s c-type: "short" size: 2 alignment: 2 class: INTEGER)
    (#\S c-type: "unsigned short" size: 2 alignment: 2 class: INTEGER)
    (#\i c-type: "int" size: 4 alignment: 4 class: INTEGER)
    (#\I c-type: "unsigned int" size: 4 alignment: 4 class: INTEGER)
    (#\l c-type: "int" size: 4 alignment: 4 class: INTEGER)
    (#\L c-type: "unsigned int" size: 4 alignment: 4 class: INTEGER)
    (#\q c-type: "long long" size: 8 alignment: 8 class: INTEGER)
    (#\Q c-type: "unsigned long long" size: 8 alignment: 8 class: INTEGER)

    ;; Floating-point types
    (#\f c-type: "float" size: 4 alignment: 4 class: SSE)
    (#\d c-type: "double" size: 8 alignment: 8 class: SSE)

    ;; Pointer types
    (#\* c-type: "char*" size: 8 alignment: 8 class: INTEGER)
    (#\@ c-type: "id" size: 8 alignment: 8 class: INTEGER)
    (#\# c-type: "Class" size: 8 alignment: 8 class: INTEGER)
    (#\: c-type: "SEL" size: 8 alignment: 8 class: INTEGER)
    (#\^ c-type: "void*" size: 8 alignment: 8 class: INTEGER)

    ;; Unknown/function pointer
    (#\? c-type: "void*" size: 8 alignment: 8 class: INTEGER)
    ))

(define (type-info code keyword)
  (let ((type-specific-info (cdr (assq code *type-info*))))
    (cadr (memq keyword type-specific-info))))

(define (classify objc-type-code)
  (type-info (string-ref objc-type-code 0) class:))

(define (sizeof objc-type-code)
  (type-info (string-ref objc-type-code 0) size:))

