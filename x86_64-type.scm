(import (srfi lists))
(export
  parse-type
  parse-function-signature

  make-type
  type?
  type-c-name
  type-size
  type-alignment
  type-signed?
  type-members

  parse-type/internal
  )

(define-type type
  constructor: make-type/internal

  (c-name read-only:)
  (size read-only:)
  (alignment read-only:)
  (signed? read-only:)
  (members read-only:))

(define (make-type #!key c-name size alignment signed members)
  (make-type/internal c-name size alignment signed members))

(define *type-info*
  `(
    ;; Boolean
    (#\B . ,(make-type c-name: "_Bool" size: 1 alignment: 1))

    ;; Integral types
    (#\c . ,(make-type c-name: "char" size: 1 alignment: 1 signed: #t))
    (#\C . ,(make-type c-name: "unsigned char" size: 1 alignment: 1 signed: #f))
    (#\s . ,(make-type c-name: "short" size: 2 alignment: 2 signed: #t))
    (#\S . ,(make-type c-name: "unsigned short" size: 2 alignment: 2 signed: #f))
    (#\i . ,(make-type c-name: "int" size: 4 alignment: 4 signed: #t))
    (#\I . ,(make-type c-name: "unsigned int" size: 4 alignment: 4 signed: #f))
    (#\l . ,(make-type c-name: "int" size: 4 alignment: 4 signed: #t))
    (#\L . ,(make-type c-name: "unsigned int" size: 4 alignment: 4 signed: #f))
    (#\q . ,(make-type c-name: "long long" size: 8 alignment: 8 signed: #t))
    (#\Q . ,(make-type c-name: "unsigned long long" size: 8 alignment: 8 signed: #f))

    ;; Floating-point types
    (#\f . ,(make-type c-name: "float" size: 4 alignment: 4))
    (#\d . ,(make-type c-name: "double" size: 8 alignment: 8))

    ;; Pointer types
    (#\* . ,(make-type c-name: "char*" size: 8 alignment: 8))
    (#\@ . ,(make-type c-name: "id" size: 8 alignment: 8))
    (#\# . ,(make-type c-name: "Class" size: 8 alignment: 8))
    (#\: . ,(make-type c-name: "SEL" size: 8 alignment: 8))
    (#\^ . ,(make-type c-name: "void*" size: 8 alignment: 8))

    ;; Unknown/function pointer
    (#\? . ,(make-type c-name: "void*" size: 8 alignment: 8))
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
			  #f)))
	(cons
	  remaining-chars
	  (make-type
	    c-name: c-type
	    members: members
	    size: ((aggregate-kind-compute-size kind) members)
	    alignment: alignment))))

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

(define (parse-function-signature signature)
  (define (digit? c)
    (and (char>=? c #\0)
	 (char<=? c #\9)))

  (define (drop-leading-digits char-list)
    (cond
      ((null? char-list)
       char-list)
      ((digit? (car char-list))
       (drop-leading-digits (cdr char-list)))
      (else
       char-list)))

  (let loop ((chars (string->list signature))
	     (type-list '()))
    (if (null? chars)
      (reverse type-list)
      (let* ((type-result (parse-type/internal chars))
	     (next-chars (drop-leading-digits (car type-result)))
	     (type (cdr type-result)))
	(loop next-chars (cons type type-list))))))
