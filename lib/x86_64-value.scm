(include "x86_64-value#.scm")

(define (scheme-object->words type object)
  `((gp . ,object)))
