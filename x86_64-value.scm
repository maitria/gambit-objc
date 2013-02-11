(export
  scheme-object->words)

(define (scheme-object->words type object)
  `((gp . ,object)))
