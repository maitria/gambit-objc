(export
  make-trampoline
  trampoline-gp-set!
  trampoline-gp-ref
  trampoline-sse-set!
  trampoline-sse-ref
  trampoline-imp-set!
  trampoline-imp-ref
  trampoline-stack-set-size!
  trampoline-stack-set-qword!
  trampoline-invoke!)

(c-declare #<<END_OF_C_DECLARE

/* The layout of this structure can't be changed without changing the assembly
 * in TRAMPOLINE-INVOKE. */
typedef struct TRAMPOLINE {
  void (* imp) ();
  unsigned long gp[6];
  double sse[8];
  unsigned long stack_size;
  unsigned long *stack;
} TRAMPOLINE;

static ___SCMOBJ destroy_trampoline(void *ptr)
{
  TRAMPOLINE *trampoline = (TRAMPOLINE *)ptr;
  if (trampoline) {
    if (trampoline->stack)
      free(trampoline->stack);
    free(trampoline);
  }
  return ___NUL;
}

END_OF_C_DECLARE
)

(c-define-type trampoline (pointer (struct "TRAMPOLINE") (objc.TRAMPOLINE) "destroy_trampoline"))

(define make-trampoline
  (c-lambda ()
	    trampoline
#<<END_OF_CODE
  ___result = (TRAMPOLINE*)malloc(sizeof(struct TRAMPOLINE));
  memset(___result, 0, sizeof(struct TRAMPOLINE));
END_OF_CODE
))

(define-macro (array-accessors #!key for value-type upper-bound)
  `(begin
     (define (,(string->symbol (string-append "trampoline-" for "-set!")) trampoline index value)
       (define set!/internal
	 (c-lambda (trampoline int ,value-type)
		   void
	   ,(string-append "___arg1->" for "[___arg2] = ___arg3;")))
       (cond
	 ((or (>= index ,upper-bound)
	      (< index 0))
	  (raise ,(string-append "invalid " for " index")))
	 (else
	  (set!/internal trampoline index value))))

     (define (,(string->symbol (string-append "trampoline-" for "-ref")) trampoline index)
       (define ref/internal
	 (c-lambda (trampoline int)
		   ,value-type
	   ,(string-append "___result = ___arg1->" for "[___arg2];")))
       (cond
	 ((or (>= index ,upper-bound)
	      (< index 0))
	  (raise ,(string-append "invalid " for " index")))
	 (else
	  (ref/internal trampoline index))))
     ))

(array-accessors for: "gp" value-type: unsigned-int64 upper-bound: 6)
(array-accessors for: "sse" value-type: double upper-bound: 8)

(define trampoline-imp-set!
  (c-lambda (trampoline unsigned-int64)
	    void
    "___arg1->imp = (void (*)()) ___arg2;"))

(define trampoline-imp-ref
  (c-lambda (trampoline)
	    unsigned-int64
    "___result = (unsigned long)___arg1->imp;"))

(define (trampoline-stack-set-size! trampoline new-size)
  (define set-size!/internal
    (c-lambda (trampoline unsigned-int64)
	      void
      #<<END_OF_CODE
	___arg1->stack_size = ___arg2;
	if (___arg1->stack)
	  free(___arg1->stack);
	___arg1->stack = (unsigned long*)malloc(sizeof(unsigned long) * ___arg2);
	memset(___arg1->stack, 0, sizeof(unsigned long) * ___arg2);
END_OF_CODE
      ))
  (if (>= new-size 16)
    (raise "current implementation cannot grow stack beyond red zone"))
  (set-size!/internal trampoline new-size))

(define (trampoline-stack-set-qword! trampoline index value)
  (define stack-size/internal
    (c-lambda (trampoline)
	      unsigned-int64
      "___result = ___arg1->stack_size;"))
  (define set!/internal
    (c-lambda (trampoline unsigned-int64 unsigned-int64)
	      void
      "___arg1->stack[___arg2] = ___arg3;"))
  (if (>= index (stack-size/internal trampoline))
    (raise "TRAMPOLINE-STACK-SET-QWORD! received index greater than stack size")
    (set!/internal trampoline index value)))

(define trampoline-invoke!
  (c-lambda (trampoline)
	    void
#<<END_OF_CODE

__asm__(
	"/* TRAMPOLINE-INVOKE! */\n"
	"pushq %%r12\n"
	"mov 120(%0),%%r12\n"
	"mov %%r12,%%rcx\n"
	"shl $3,%%r12;\n"
	"mov 128(%0),%%rsi\n"
	"mov %%rsp,%%rdi\n"
	"sub %%r12,%%rdi\n"
	"cld\n"
	"rep movsq\n"

	"mov 8(%0),%%rdi;\n"
	"mov 16(%0),%%rsi;\n"
	"mov 24(%0),%%rdx;\n"
	"mov 32(%0),%%rcx;\n"
	"mov 40(%0),%%r8;\n"
	"mov 48(%0),%%r9;\n"
	"movq 56(%0),%%xmm0;\n"
	"movq 64(%0),%%xmm1;\n"
	"movq 72(%0),%%xmm2;\n"
	"movq 80(%0),%%xmm3;\n"
	"movq 88(%0),%%xmm4;\n"
	"movq 96(%0),%%xmm5;\n"
	"movq 104(%0),%%xmm6;\n"
	"movq 112(%0),%%xmm7;\n"
	"sub %%r12,%%rsp;\n"
	"call *0(%0);\n"
	"add %%r12,%%rsp;\n"
	"mov %%rax,8(%0);\n"
	"mov %%rdx,16(%0);\n"
	"movq %%xmm0,56(%0);\n"
	"movq %%xmm1,64(%0);\n"
	"popq %%r12;\n"
       :
       : "r"(___arg1)
       : "%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9",
         "%xmm0", "%xmm1", "%xmm2", "%xmm3",
	 "%xmm4", "%xmm5", "%xmm6", "%xmm7",
	 "%rax", "%r10", "%r11",
	 "%xmm8", "%xmm9", "%xmm10", "%xmm11",
	 "%xmm12", "%xmm13", "%xmm14", "%xmm15"
       );

END_OF_CODE
))

