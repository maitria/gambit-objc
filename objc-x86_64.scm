(export
  sizeof)

(define (sizeof objc-type-code)
  (case objc-type-code
   ((#\c #\C) 1)
   ((#\s #\S) 2)
   ((#\i #\l #\I) 4)
   ((#\q) 8)))
