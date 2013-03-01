(include "../lib/expect#.scm")
(include "../lib/x86_64-type#.scm")
(include "../lib/x86_64-value#.scm")

(expect (equal? '((gp . 42)) (scheme-object->words (parse-type "i") 42)))

(display-expect-results)
