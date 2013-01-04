(export
  classify
  sizeof)

(define *type-info*
  '((#\c c-type: "char" size: 1 alignment: 1 class: INTEGER)
    (#\C c-type: "unsigned char" size: 1 alignment: 1 class: INTEGER)
    (#\B c-type: "_Bool" size: 1 alignment: 1 class: INTEGER)
    (#\s c-type: "short" size: 2 alignment: 2 class: INTEGER)
    (#\S c-type: "unsigned short" size: 2 alignment: 2 class: INTEGER)
    (#\i c-type: "int" size: 4 alignment: 4 class: INTEGER)
    (#\I c-type: "unsigned int" size: 4 alignment: 4 class: INTEGER)
    (#\l c-type: "int" size: 4 alignment: 4 class: INTEGER)
    (#\L c-type: "unsigned int" size: 4 alignment: 4 class: INTEGER)
    ))

(define (type-info code keyword)
  (let ((type-specific-info (cdr (assq code *type-info*))))
    (cadr (memq keyword type-specific-info))))

(define (classify objc-type-code)
  (case (string-ref objc-type-code 0)
   ((#\q #\S #\Q #\* #\@ #\# #\: #\^ #\?) 'INTEGER)
   ((#\f #\d) 'SSE)
   (else (type-info (string-ref objc-type-code 0) class:))))

(define (sizeof objc-type-code)
  (case (string-ref objc-type-code 0)
   ((#\f) 4)
   ((#\q #\Q #\d #\* #\@ #\# #\: #\^ #\?) 8)
   (else (type-info (string-ref objc-type-code 0) size:))))

