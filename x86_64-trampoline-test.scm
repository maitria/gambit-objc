(import expect)
(import x86_64-trampoline)

(let ((t (make-trampoline)))
  (trampoline-gp-set! t 0 78)
  (expect (= 78 (trampoline-gp-ref t 0))))

(let ((t (make-trampoline)))
  (trampoline-sse-set! t 2 2.0)
  (expect (= 2.0 (trampoline-sse-ref t 2))))

(let ((t (make-trampoline)))
  (trampoline-imp-set! t 978654321)
  (expect (= 978654321 (trampoline-imp-ref t))))

(c-declare #<<END_OF_CODE

static unsigned long the_passed_ulongs[6] = {};
static void six_integers(
    unsigned long i0, unsigned long i1, unsigned long i2,
    unsigned long i3, unsigned long i4, unsigned long i5
    )
{
  the_passed_ulongs[0] = i0;
  the_passed_ulongs[1] = i1;
  the_passed_ulongs[2] = i2;
  the_passed_ulongs[3] = i3;
  the_passed_ulongs[4] = i4;
  the_passed_ulongs[5] = i5;
}

static double the_passed_doubles[8] = {};
static void eight_doubles(
    double i0, double i1, double i2, double i3,
    double i4, double i5, double i6, double i7
    )
{
  the_passed_doubles[0] = i0;
  the_passed_doubles[1] = i1;
  the_passed_doubles[2] = i2;
  the_passed_doubles[3] = i3;
  the_passed_doubles[4] = i4;
  the_passed_doubles[5] = i5;
  the_passed_doubles[6] = i6;
  the_passed_doubles[7] = i7;
}

static unsigned long returns_a_ulong()
{
  return 0xDEADBEEFDEADBEEFUL;
}

struct sixteenbyte
{
  unsigned long a,b;
};

static struct sixteenbyte returns_a_sixteenbyte()
{
  struct sixteenbyte sb;
  sb.a = 0xDEADBEEFDEADBEEFUL;
  sb.b = 0xFDFDFDFDFDFDFDFDUL;
  return sb;
}

struct twodouble
{
  double a;
  double b;
};

static struct twodouble returns_a_twodouble()
{
  struct twodouble td;
  td.a = 12.8;
  td.b = 40.96;
  return td;
}

struct stackstruct
{
  unsigned long a,b,c;
};

static struct stackstruct the_passed_stackstruct;
static void takes_a_stackstruct(struct stackstruct ms)
{
  memcpy(&the_passed_stackstruct, &ms, sizeof(struct stackstruct));
}

END_OF_CODE
)

(define-macro (address-of c-thing)
  `((c-lambda ()
	      unsigned-int64 
      ,(string-append "___result = (unsigned long)" c-thing ";"))))

(define gp-parameter-received
  (c-lambda (int)
	    unsigned-int64
    "___result = the_passed_ulongs[___arg1];"))

(define sse-parameter-received
  (c-lambda (int)
	    double
    "___result = the_passed_doubles[___arg1];"))

(define (correctly-passes-gp? n)
  (let ((t (make-trampoline)))
    (trampoline-imp-set! t (address-of "six_integers"))
    (trampoline-gp-set! t n 42)
    (trampoline-invoke! t)
    (= 42 (gp-parameter-received n))))

(expect (correctly-passes-gp? 0))
(expect (correctly-passes-gp? 1))
(expect (correctly-passes-gp? 2))
(expect (correctly-passes-gp? 3))
(expect (correctly-passes-gp? 4))
(expect (correctly-passes-gp? 5))

(expect (raises? (lambda () (trampoline-gp-set! (make-trampoline) 6 99))))
(expect (raises? (lambda () (trampoline-gp-set! (make-trampoline) -1 99))))
(expect (raises? (lambda () (trampoline-gp-ref (make-trampoline) 6))))
(expect (raises? (lambda () (trampoline-gp-ref (make-trampoline) -1))))

(define (correctly-passes-sse? n)
  (let ((t (make-trampoline)))
    (trampoline-imp-set! t (address-of "eight_doubles"))
    (trampoline-sse-set! t n 12.8)
    (trampoline-invoke! t)
    (= 12.8 (sse-parameter-received n))))

(expect (correctly-passes-sse? 0))
(expect (correctly-passes-sse? 1))
(expect (correctly-passes-sse? 2))
(expect (correctly-passes-sse? 3))
(expect (correctly-passes-sse? 4))
(expect (correctly-passes-sse? 5))
(expect (correctly-passes-sse? 6))
(expect (correctly-passes-sse? 7))

(expect (raises? (lambda () (trampoline-sse-set! (make-trampoline) 8 12.8))))
(expect (raises? (lambda () (trampoline-sse-set! (make-trampoline) -1 12.8))))
(expect (raises? (lambda () (trampoline-sse-ref (make-trampoline) 8))))
(expect (raises? (lambda () (trampoline-sse-ref (make-trampoline) -1))))

(let ((t (make-trampoline)))
  (trampoline-imp-set! t (address-of "returns_a_ulong"))
  (trampoline-invoke! t)
  (expect (= #xDEADBEEFDEADBEEF (trampoline-gp-ref t 0))))

(let ((t (make-trampoline)))
  (trampoline-imp-set! t (address-of "returns_a_sixteenbyte"))
  (trampoline-invoke! t)
  (expect (= #xDEADBEEFDEADBEEF (trampoline-gp-ref t 0)))
  (expect (= #xFDFDFDFDFDFDFDFD (trampoline-gp-ref t 1))))

(let ((t (make-trampoline)))
  (trampoline-imp-set! t (address-of "returns_a_twodouble"))
  (trampoline-invoke! t)
  (expect (= 12.8 (trampoline-sse-ref t 0)))
  (expect (= 40.96 (trampoline-sse-ref t 1))))

(let ((t (make-trampoline)))
  (trampoline-imp-set! t (address-of "takes_a_stackstruct"))
  (trampoline-stack-set-size! t 4)
  (trampoline-stack-set-qword! t 0 #xDEADBEEFDEADBEEF)
  (trampoline-stack-set-qword! t 1 #xDFDFDFDFDFDFDFDF)
  (trampoline-stack-set-qword! t 2 #xBABABABABABABABA)
  (trampoline-invoke! t)
  (let ((a ((c-lambda () unsigned-int64 "___result = the_passed_stackstruct.a;")))
        (b ((c-lambda () unsigned-int64 "___result = the_passed_stackstruct.b;")))
        (c ((c-lambda () unsigned-int64 "___result = the_passed_stackstruct.c;"))))
    (expect (= #xDEADBEEFDEADBEEF a))
    (expect (= #xDFDFDFDFDFDFDFDF b))
    (expect (= #xBABABABABABABABA c))))

(expect "refuses to set a negative stack size"
  (raises? (lambda () (trampoline-stack-set-size! (make-trampoline) -1))))
(expect "current implementation refuses to grow stack beyond red zone"
  (raises? (lambda () (trampoline-stack-set-size! (make-trampoline) 16))))

(display-expect-results)
