(define-module (hui packages wm)
  #:use-module (gnu packages)
  #:use-module (gnu packages wm)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu))

(define-public dwl-hui
  (package
    (inherit dwl)
    (name "dwl-hui")
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags
       (list
        (string-append "CC=" ,(cc-for-target))
        (string-append "PREFIX=" (assoc-ref %outputs "out"))
        (string-append "CFLAGS=" "-DWAYLAND -lxcb -lxcb-icccm")) ; enable xwayland
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))))
