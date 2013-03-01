(namespace ("objc#"

  selector?
  object?

  class
  string->selector
  selector->string
  call-method

  extract-selector-name-from-arg-list
  extract-args-from-arg-list
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
