(export
  make-trampoline
  trampoline-gp-set!
  trampoline-gp-ref
  trampoline-sse-set!
  trampoline-sse-ref
  trampoline-imp-set!
  trampoline-imp-ref
  trampoline-invoke)

(c-declare #<<END_OF_C_DEFINE

/* The layout of this structure can't be changed without changing the assembly
 * in TRAMPOLINE-INVOKE. */
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

(define trampoline-invoke
  (c-lambda (trampoline)
	    void
#<<END_OF_CODE

__asm__(
	"/* TRAMPOLINE-INVOKE */\n"
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
	"call *0(%0);\n"
	"mov %%rax,8(%0);\n"
	"mov %%rdx,16(%0);\n"
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
	    
