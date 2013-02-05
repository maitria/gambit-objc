(import expect)
(import x86_64-load)
(import x86_64-trampoline)

(expect (= 79 (trampoline-imp-ref (load-trampoline address: 79))))

(display-expect-results)
