(export
  classify
  sizeof)

(define *type-info*
  '((#\c c-type: "char" size: 1 alignment: 1 class: INTEGER)
    (#\C c-type: "unsigned char" size: 1 alignment: 1 class: INTEGER)
    (#\B c-type: "_Bool" size: 1 alignment: 1 class: INTEGER)))

(define (type-info code keyword)
  (let ((type-specific-info (cdr (assq code *type-info*))))
    (cadr (memq keyword type-specific-info))))

(define (classify objc-type-code)
  (case (string-ref objc-type-code 0)
   ((#\c #\i #\l #\s #\q #\C #\I #\S #\L #\Q #\B #\* #\@ #\# #\: #\^ #\?) 'INTEGER)
   ((#\f #\d) 'SSE)))

(define (sizeof objc-type-code)
  (case (string-ref objc-type-code 0)
   ((#\c #\C #\B) (type-info (string-ref objc-type-code 0) size:))
   ((#\s #\S) 2)
   ((#\i #\l #\I #\L #\f) 4)
   ((#\q #\Q #\d #\* #\@ #\# #\: #\^ #\?) 8)))

