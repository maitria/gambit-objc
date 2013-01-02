(import expect)
(import objc-x86_64)

(expect (= 1 (sizeof #\c)))
(expect (= 4 (sizeof #\i)))
(expect (= 2 (sizeof #\s)))
(expect (= 4 (sizeof #\l)))

(display-expect-results)
