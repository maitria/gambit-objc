(namespace ("objc#"
  ;; Public interface
  class 
  import-classes
  object?
  selector?
  string->selector

  ;; Internal stuff
  call-method
  extract-args-from-arg-list
  extract-selector-name-from-arg-list
  make-object-tags
  make-selector-tags
  raw-class
  raw-object?
  raw-object->object
  *object-table*
  ))

(define-macro (import-classes class-list)
  (let class-loop ((resulting-syntax '(begin))
		   (rest-of-classes class-list))
    (if (null? rest-of-classes)
      (reverse resulting-syntax)
      (let ((class-name (car rest-of-classes)))
	(class-loop
	  (cons `(define ,class-name (class ,(symbol->string class-name))) resulting-syntax)
	  (cdr rest-of-classes))))))
