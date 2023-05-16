(use-modules
 (gnu)
 (gnu packages)
 (guix packages)
 (guix build-system copy)
 (guix gexp)
 ((guix licenses) #:prefix license:)
 (guix utils)
 (guix download))

;; need replace by source
(define-public clash
  (package
    (name "clash")
    (version "1.15.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/"
                                  "Dreamacro/clash/releases/download/v"
                                  version
                                  "/clash-linux-amd64-v"
                                  version
                                  ".gz"))
              (sha256
               (base32
                "1sq7gs4in4vihkz7ih4lclpdfg9qq4xfm0bafhxvx1l5i8lb1dxb"))))
    (build-system copy-build-system)
    (home-page "https://github.com/Dreamacro/clash")
    (arguments
     `(#:install-plan '((,(string-append "clash-linux-amd64-v" version)
                         "bin/clash"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'fix-perm
           (lambda* (#:key outputs #:allow-other-keys)
             (chmod (string-append (assoc-ref outputs "out")
                                   "/bin/clash") #o755))))))
    (synopsis "Proxy")
    (description "Proxy")
    (license #f)))
