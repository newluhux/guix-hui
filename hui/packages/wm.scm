(define-module (hui packages wm)
  #:use-module (gnu packages)
  #:use-module (gnu packages wm)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu))

(define-public dwl-hui
  (package
    (inherit dwl)
    (name "dwl-hui")
    (version "0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/djpohly/dwl")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pj0h3zd2f60hxpavpmgzid1sj7hf9m5cgclbackljqq4gpwlvir"))
              (patches (list (local-file "aux-files/dwl/0001-config.def.h-default-use-floating-layout.patch")))))
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags
       (list
        (string-append "CC=" ,(cc-for-target))
        (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'enable-xwayland
           (lambda _
             (substitute* "config.def.h"
               (("\"foot\"") "\"sakura\""))
             (substitute* "config.mk"
               (("^XWAYLAND = $") "")
               (("^XLIB = $") "")
               (("^#XWAYLAND") "XWAYLAND")
               (("^#XLIBS") "XLIBS")))))))))

dwl-hui
