(import (srfi lists))
(export
  parse-type

  make-type
  type?
  type-info
  type-c-name
  type-size
  type-alignment
  type-class
  type-signed?
  type-members

  make-trampoline
  trampoline-gp-set!
  trampoline-gp-ref
  trampoline-sse-set!
  trampoline-sse-ref
  trampoline-imp-set!
  trampoline-imp-ref

  parse-type/internal
  reduce-classification/internal
  )

(define-type type
  constructor: make-type/internal

  (c-name read-only:)
  (size read-only:)
  (alignment read-only:)
  (class read-only:)
  (signed? read-only:)
  (members read-only:))

(define (make-type #!key c-name size alignment class signed members)
  (make-type/internal c-name size alignment class signed members))

(define (type-info type keyword)
  (case keyword
   ((c-name:) (type-c-name type))
   ((class:) (type-class type))
   ((signed:) (type-signed? type))
   ((size:) (type-size type))
   ((alignment:) (type-alignment type))
   ((members:) (type-members type))))

(define *type-info*
  `(
    ;; Boolean
    (#\B . ,(make-type c-name: "_Bool" size: 1 alignment: 1 class: 'INTEGER))

    ;; Integral types
    (#\c . ,(make-type c-name: "char" size: 1 alignment: 1 class: 'INTEGER signed: #t))
    (#\C . ,(make-type c-name: "unsigned char" size: 1 alignment: 1 class: 'INTEGER signed: #f))
    (#\s . ,(make-type c-name: "short" size: 2 alignment: 2 class: 'INTEGER signed: #t))
    (#\S . ,(make-type c-name: "unsigned short" size: 2 alignment: 2 class: 'INTEGER signed: #f))
    (#\i . ,(make-type c-name: "int" size: 4 alignment: 4 class: 'INTEGER signed: #t))
    (#\I . ,(make-type c-name: "unsigned int" size: 4 alignment: 4 class: 'INTEGER signed: #f))
    (#\l . ,(make-type c-name: "int" size: 4 alignment: 4 class: 'INTEGER signed: #t))
    (#\L . ,(make-type c-name: "unsigned int" size: 4 alignment: 4 class: 'INTEGER signed: #f))
    (#\q . ,(make-type c-name: "long long" size: 8 alignment: 8 class: 'INTEGER signed: #t))
    (#\Q . ,(make-type c-name: "unsigned long long" size: 8 alignment: 8 class: 'INTEGER signed: #f))

    ;; Floating-point types
    (#\f . ,(make-type c-name: "float" size: 4 alignment: 4 class: 'SSE))
    (#\d . ,(make-type c-name: "double" size: 8 alignment: 8 class: 'SSE))

    ;; Pointer types
    (#\* . ,(make-type c-name: "char*" size: 8 alignment: 8 class: 'INTEGER))
    (#\@ . ,(make-type c-name: "id" size: 8 alignment: 8 class: 'INTEGER))
    (#\# . ,(make-type c-name: "Class" size: 8 alignment: 8 class: 'INTEGER))
    (#\: . ,(make-type c-name: "SEL" size: 8 alignment: 8 class: 'INTEGER))
    (#\^ . ,(make-type c-name: "void*" size: 8 alignment: 8 class: 'INTEGER))

    ;; Unknown/function pointer
    (#\? . ,(make-type c-name: "void*" size: 8 alignment: 8 class: 'INTEGER))
    ))

(define (ignorable? char)
  (memq char '(#\r #\n #\N #\o #\O #\R #\V)))

(define-type aggregate-kind
  (name read-only:)
  (open-bracket read-only:)
  (close-bracket read-only:)
  (compute-size read-only:))

(define (compute-struct-size members)
  (if members
    (fold
      (lambda (type size)
	(let* ((alignment (type-alignment type))
	       (padding (if (= 0 (modulo size alignment))
			  0
			  (- alignment (modulo size alignment)))))
	(+ size padding (type-size type))))
      0 members)
    #f))

(define *struct-kind*
  (make-aggregate-kind
    "struct"
    #\{
    #\}
    compute-struct-size))

(define (compute-union-size members)
  (if members
    (apply max (map type-size members))
    #f))

(define *union-kind*
  (make-aggregate-kind
    "union"
    #\(
    #\)
    compute-union-size))

(define (reduce-classification/internal left right)
  (cond
    ((eq? left right)
     left)
    ((eq? left 'NO_CLASS)
     right)
    ((eq? right 'NO_CLASS)
     left)
    ((or (eq? left 'MEMORY)
         (eq? right 'MEMORY))
     'MEMORY)
    ((or (eq? left 'INTEGER)
         (eq? right 'INTEGER))
     'INTEGER)
    (else
     'SSE)))
      
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
                (after-=? #f)
                (nesting-level 0))
   (cond
     ((and (not after-=?)
           (char=? #\= (car chars)))
      (continue
        (cdr chars)
        name-chars
        defn-chars
        #t nesting-level))

     ((and (= 0 nesting-level)
           (char=? (aggregate-kind-close-bracket kind) (car chars)))
      (let* ((remaining-chars (cdr chars))
             (name (list->string (reverse name-chars)))
             (c-type (string-append (aggregate-kind-name kind) " " name))
             (members (if after-=?
                        (parse-aggregate-type-members (reverse defn-chars))
                        #f))
	     (alignment (if members
			  (apply lcm (map type-alignment members))
			  #f))
	     (class 'MEMORY))
      (cons
	remaining-chars
	(make-type
	  c-name: c-type
	  members: members
	  size: ((aggregate-kind-compute-size kind) members)
	  alignment: alignment
	  class: class))))

     ((and after-=?
           (char=? (aggregate-kind-open-bracket kind) (car chars)))
      (continue
        (cdr chars)
        name-chars
        (cons (car chars) defn-chars)
        after-=?
        (+ nesting-level 1)))

     ((and after-=?
           (char=? (aggregate-kind-close-bracket kind) (car chars)))
      (continue
        (cdr chars)
        name-chars
        (cons (car chars) defn-chars)
        after-=?
        (- nesting-level 1)))

     (after-=?
      (continue
        (cdr chars)
        name-chars
        (cons (car chars) defn-chars)
        after-=?
        nesting-level))

     (else
      (continue
        (cdr chars)
        (cons (car chars) name-chars)
        defn-chars
        after-=?
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

;; Trampoline
(c-declare #<<END_OF_C_DEFINE

typedef struct TRAMPOLINE {
  void (* imp) ();
  unsigned long gp[6];
  double sse[8];
} TRAMPOLINE;

END_OF_C_DEFINE
)

(c-define-type trampoline (pointer (struct "TRAMPOLINE")))

(define make-trampoline
  (c-lambda ()
	    trampoline
#<<END_OF_CODE
  ___result = (TRAMPOLINE*)malloc(sizeof(struct TRAMPOLINE));
  memset(___result, 0, sizeof(struct TRAMPOLINE));
END_OF_CODE
))

(define trampoline-gp-set!
  (c-lambda (trampoline int unsigned-int64)
	    void
    "___arg1->gp[___arg2] = ___arg3;"))

(define trampoline-gp-ref
  (c-lambda (trampoline int)
	    unsigned-int64
    "___result = ___arg1->gp[___arg2];"))

(define trampoline-sse-set!
  (c-lambda (trampoline int double)
	    void
    "___arg1->sse[___arg2] = ___arg3;"))

(define trampoline-sse-ref
  (c-lambda (trampoline int)
	    double
    "___result = ___arg1->sse[___arg2];"))

(define trampoline-imp-set!
  (c-lambda (trampoline unsigned-int64)
	    void
    "___arg1->imp = (void (*)()) ___arg2;"))

(define trampoline-imp-ref
  (c-lambda (trampoline)
	    unsigned-int64
    "___result = (unsigned long)___arg1->imp;"))
