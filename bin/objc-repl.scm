(include "~~lib/_gambit#.scm")
(macro-readtable-bracket-keyword-set! ##main-readtable ':)

(let* ((repl-path (path-directory (car (command-line))))
       (objc#-path (string-append repl-path "../lib/objc#.scm")))
  (eval `(##include ,objc#-path)))

(##repl-debug-main)
