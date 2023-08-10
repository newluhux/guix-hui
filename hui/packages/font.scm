(define-module (hui packages font)
  #:use-module (gnu packages)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages fontutils)
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

(define-public font-unscii
  (package
    (name "font-unscii")
    (version "2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://viznut.fi/unscii/unscii-"
                           version "-src.tar.gz"))
       (sha256
        (base32 "0msvqrq7x36p76a2n5bzkadh95z954ayqa08wxd017g4jpa1a4jd"))))
    (build-system gnu-build-system)
    (native-inputs
     (list perl bdftopcf perl-text-charwidth sdl sdl-image fontforge))
    (arguments
     `(#:tests? #f          ; no check target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((ttf (string-append (assoc-ref outputs "out")
                                        "/share/fonts/truetype/")))
               (install-file "unscii-16-full.ttf" ttf)
               (install-file "unscii-16.ttf" ttf)
               (install-file "unscii-8-thin.ttf" ttf)
               (install-file "unscii-8.ttf" ttf)))))))
    (synopsis
     "Unscii is a set of bitmapped Unicode fonts based on classic system fonts")
    (description
     "Unscii attempts to support character cell art well while also being
suitable for terminal and programming use.")
     (home-page "http://viznut.fi/unscii/")
     (license #f)))

font-unscii
