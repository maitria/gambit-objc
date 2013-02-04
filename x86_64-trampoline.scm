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

(c-declare #<<END_OF_C_DEFINE

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

END_OF_C_DEFINE
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

(define trampoline-gp-set!
  (c-lambda (trampoline int unsigned-int64)
	    void
#<<END_OF_C_LAMBDA
  if (___arg2 < 0 || ___arg2 >= sizeof(___arg1->gp)/sizeof(___arg1->gp[0]))  
    ___err = ___FIX(___UNKNOWN_ERR);
  else
    ___arg1->gp[___arg2] = ___arg3;
END_OF_C_LAMBDA
))

(define (trampoline-gp-ref trampoline index)
  (define ref/internal
    (c-lambda (trampoline int)
	      unsigned-int64
      "___result = ___arg1->gp[___arg2];"))
  (cond
    ((or (>= index 6)
	 (< index 0))
     (raise "invalid gp index"))
    (else
     (ref/internal trampoline index))))

(define trampoline-sse-set!
  (c-lambda (trampoline int double)
	    void
#<<END_OF_C_LAMBDA
  if (___arg2 < 0 || ___arg2 >= sizeof(___arg1->sse)/sizeof(___arg1->sse[0]))
    ___err = ___FIX(___UNKNOWN_ERR);
  else
    ___arg1->sse[___arg2] = ___arg3;
END_OF_C_LAMBDA
))

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

(define trampoline-stack-set-size!
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

(define trampoline-stack-set-qword!
  (c-lambda (trampoline unsigned-int64 unsigned-int64)
	    void
#<<END_OF_CODE
  ___arg1->stack[___arg2] = ___arg3;
END_OF_CODE
))

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

