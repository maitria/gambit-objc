(export
  sizeof)

(define (sizeof objc-type-code)
  (case objc-type-code
   ((#\c #\C #\B) 1)
   ((#\s #\S) 2)
   ((#\i #\l #\I #\L #\f) 4)
   ((#\q #\Q #\d #\* #\@ #\# #\: #\^ #\?) 8)))
