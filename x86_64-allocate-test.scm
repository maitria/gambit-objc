(import expect)
(import x86_64-allocate)
(import x86_64-trampoline)

(expect (= 79 (trampoline-imp-ref (trampoline-allocate address: 79))))

(display-expect-results)
