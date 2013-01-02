(export
  sizeof)

(define (sizeof objc-type-code)
  (case objc-type-code
   ((#\c) 1)
   ((#\i) 4)))
