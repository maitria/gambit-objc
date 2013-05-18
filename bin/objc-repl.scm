(include "../lib/objc#.scm")
(enable-objc-braces)

(define (path)
  (define path-string (getenv "PATH"))
  (define path-parts '())

  (let loop ((start 0)
	     (offset 0))
    (cond
      ((or (= offset (string-length path-string))
	   (char=? #\: (string-ref path-string offset)))
       (set! path-parts (cons (string-append
				(substring path-string start offset)
				"/")
			      path-parts))
       (if (= offset (string-length path-string))
	 #f
	 (loop (+ offset 1)
	       (+ offset 1))))
      (else
       (loop start
	     (+ offset 1)))))

  (reverse path-parts))


(define (locate-ourselves)
  (let ((specified-directory (path-directory (car (command-line)))))
    (cond
      ((string=? "" specified-directory)
       (call/cc
	 (lambda (return)
	   (for-each
	     (lambda (path-part)
	       (define possible-path (string-append path-part (car (command-line))))
	       (if (file-exists? possible-path)
		 (return path-part)))
	     (path))
	   "?/")))
      (else
       (path-expand specified-directory)))))

(eval `(##include ,(string-append (locate-ourselves) "../lib/objc#.scm")))
(##repl-debug-main)
