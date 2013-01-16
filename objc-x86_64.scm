(export
  parse-type

  type-info
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


(define (parse-type encoded-type)
  (cdr (%%parse-type (string->list encoded-type))))

(define (%%parse-type encoded-type-chars)
  (define (ignorable? char)
    (memq char '(#\r #\n #\N #\o #\O #\R #\V)))
  (let ((current-char (car encoded-type-chars)))
    (cond
      ((ignorable? current-char)
       (%%parse-type (cdr encoded-type-chars)))
      (else
       (cons
         (cdr encoded-type-chars)
	 (cdr (assq current-char *type-info*)))))))

(define (type-info type keyword)
  (cadr (memq keyword type)))

(define (type-class type)
  (type-info type class:))

(define (type-signed? type)
  (type-info type signed:))

(define (type-size type)
  (type-info type size:))

