(import expect)
(import objc-x86_64)

(expect (= 1 (sizeof #\c)))
(expect (= 4 (sizeof #\i)))

(display-expect-results)
