(define-module (hui packages font)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public font-univga
  (package
    (name "font-univga")
    (version "2001")
    (source
     (origin
       (method url-fetch)
       (uri "https://www.inp.nsk.su/~bolkhov/files/fonts/univga/uni-vga.tgz")
       (file-name "uni-vga.tgz")
       (sha256
        (base32 "05sns8h5yspa7xkl81ri7y1yxf5icgsnl497f3xnaryhx11s2rv6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f          ; no check target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file
              "u_vga16.bdf"
              (string-append (assoc-ref outputs "out") "/share/fonts/")))))))

    (synopsis
     "UNI-VGA is a Unicode VGA font for X11 and console.")
    (description
     "UNI-VGA is a Unicode VGA font for X11 and console.
It is primarily intended to be the single source of fonts for console
and for XDosEmu.")
     (home-page "https://www.inp.nsk.su./~bolkhov/files/fonts/univga/")
     (license #f)))

font-univga

