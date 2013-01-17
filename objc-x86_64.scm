(export
  parse-type
  parse-type/internal

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

(define (ignorable? char)
  (memq char '(#\r #\n #\N #\o #\O #\R #\V)))

(define-type aggregate-kind
  (name read-only:)
  (open-bracket read-only:)
  (close-bracket read-only:))

(define *struct-kind* (make-aggregate-kind "struct" #\{ #\}))
(define *union-kind* (make-aggregate-kind "union" #\( #\)))

(define (parse-aggregate-type-members chars)
  (let loop ((chars chars)
             (member-types '()))
    (cond
      ((null? chars)
       (reverse member-types))
      (else
       (let* ((parse-result (parse-type/internal chars))
              (next-chars (car parse-result))
              (type (cdr parse-result)))
         (loop next-chars (cons type member-types)))))))

(define (parse-aggregate-type chars kind)
 (let continue ((chars chars)
                (name-chars '())
                (defn-chars '())
                (in-defn? #f)
                (nesting-level 0))
   (cond
     ((and (not in-defn?)
           (char=? #\= (car chars)))
      (continue
        (cdr chars)
        name-chars
        defn-chars
        #t
        nesting-level))

     ((and (= 0 nesting-level)
           (char=? (aggregate-kind-close-bracket kind) (car chars)))
      (let* ((remaining-chars (cdr chars))
             (struct-name (list->string (reverse name-chars)))
             (c-type (string-append (aggregate-kind-name kind) " " struct-name))
             (members (if in-defn?
                        (parse-aggregate-type-members (reverse defn-chars))
                        #f)))
      `(,remaining-chars c-type: ,c-type members: ,members)))

     ((and in-defn?
           (char=? (aggregate-kind-open-bracket kind) (car chars)))
      (continue
        (cdr chars)
        name-chars
        (cons (car chars) defn-chars)
        in-defn?
        (+ nesting-level 1)))

     ((and in-defn?
           (char=? (aggregate-kind-close-bracket kind) (car chars)))
      (continue
        (cdr chars)
        name-chars
        (cons (car chars) defn-chars)
        in-defn?
        (- nesting-level 1)))

     (in-defn?
      (continue
        (cdr chars)
        name-chars
        (cons (car chars) defn-chars)
        in-defn?
        nesting-level))

     (else
      (continue
        (cdr chars)
        (cons (car chars) name-chars)
        defn-chars
        in-defn?
        nesting-level)))))

(define (parse-type encoded-type)
  (cdr (parse-type/internal (string->list encoded-type))))

(define (parse-type/internal chars)
  (cond
    ((ignorable? (car chars))
     (parse-type/internal (cdr chars)))
    ((char=? #\{ (car chars))
     (parse-aggregate-type (cdr chars) *struct-kind*))
    ((char=? #\( (car chars))
     (parse-aggregate-type (cdr chars) *union-kind*))
    (else
     (cons
       (cdr chars)
       (cdr (assq (car chars) *type-info*))))))

(define (type-info type keyword)
  (cadr (memq keyword type)))

(define (type-class type)
  (type-info type class:))

(define (type-signed? type)
  (type-info type signed:))

(define (type-size type)
  (type-info type size:))

