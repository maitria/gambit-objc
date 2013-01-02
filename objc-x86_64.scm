(export
  sizeof)

(define (sizeof objc-type-code)
  (case objc-type-code
   ((#\c #\C) 1)
   ((#\s) 2)
   ((#\i #\l) 4)
   ((#\q) 8)))
