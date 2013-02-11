(import expect)
(import x86_64-type)
(import x86_64-value)

(expect (equal? '((gp . 42)) (scheme-object->words (parse-type "i") 42)))

(display-expect-results)
