(export
  classify
  sizeof

  parse-type
  type-class
  type-signed?
  type-size
  )

(define *type-info*
  '(
    ;; Boolean
    (#\B c-type: "_Bool" size: 1 alignment: 1 class: INTEGER)

    ;; Integral types
    (#\c c-type: "char" size: 1 alignment: 1 class: INTEGER signed: #t)
    (#\C c-type: "unsigned char" size: 1 alignment: 1 class: INTEGER signed: #f)
    (#\s c-type: "short" size: 2 alignment: 2 class: INTEGER signed: #t)
    (#\S c-type: "unsigned short" size: 2 alignment: 2 class: INTEGER signed: #f)
    (#\i c-type: "int" size: 4 alignment: 4 class: INTEGER signed: #t)
    (#\I c-type: "unsigned int" size: 4 alignment: 4 class: INTEGER signed: #f)
    (#\l c-type: "int" size: 4 alignment: 4 class: INTEGER signed: #t)
    (#\L c-type: "unsigned int" size: 4 alignment: 4 class: INTEGER signed: #f)
    (#\q c-type: "long long" size: 8 alignment: 8 class: INTEGER signed: #t)
    (#\Q c-type: "unsigned long long" size: 8 alignment: 8 class: INTEGER signed: #f)

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

(define (parse-type type-encoding offset)
  (cons
    (+ 1 offset)
    (cdr (assq (string-ref type-encoding offset) *type-info*))))

(define (type-class type)
  (cadr (memq class: type)))

(define (type-signed? type)
  (cadr (memq signed: type)))

(define (type-size type)
  (cadr (memq size: type)))

