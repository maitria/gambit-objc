(namespace ("objc#"

  selector?
  object?

  class
  string->selector
  selector->string
  call-method
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

(define-macro (setup-objc-call)
  (eval
    '(begin
       (namespace ("objc#"
	  extract-selector-name-from-arg-list
	  extract-args-from-arg-list
	  objc-call-expander
	  ))

       (define (extract-selector-name-from-arg-list args)
         (if (= 1 (length args))
           (symbol->string (car args))
           (let arg-loop ((name-so-far "")
                          (rest-of-args args))
             (if (null? rest-of-args)
                name-so-far
                (arg-loop (string-append name-so-far (keyword->string (car rest-of-args)) ":")
                          (cddr rest-of-args))))))

       (define (extract-args-from-arg-list arg-list)
         (if (= 1 (length arg-list))
           '()
           (let arg-loop ((reversed-args '())
                          (remaining-arg-list arg-list))
             (if (null? remaining-arg-list)
               (reverse reversed-args)
               (arg-loop (cons (cadr remaining-arg-list) reversed-args)
                         (cddr remaining-arg-list))))))

       (define (objc-call-expander target . arg-list)
         `(call-method
            ,target
            (string->selector ,(extract-selector-name-from-arg-list arg-list))
            ,@(extract-args-from-arg-list arg-list)))))
  '(begin))

(setup-objc-call)
(define-macro (: . args)
  (apply objc-call-expander args))


