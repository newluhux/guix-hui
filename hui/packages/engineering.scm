(define-module (hui packages engineering)
  #:use-module (gnu packages)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tls)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix download))

(define-public cutter-rizin
  (package
    (name "cutter-rizin")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/rizinorg/cutter/releases/"
			   "download/v" version "/Cutter-v" version
			   "-src.tar.gz"))
       (sha256
        (base32 "0dcz68vs0l56ykcgalcvl908n1mk0ld123ki06q0xyvzcmy03lpm"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DCUTTER_USE_BUNDLED_RIZIN=OFF")
       #:tests? #f))
    (inputs
     (list rizin qtbase-5 qtsvg-5 qttools-5 openssl))
    (home-page "https://github.com/rizinorg/cutter")
    (synopsis "Open Source Reverse Engineering Platform powered by rizin")
    (description "Cutter is a free and open-source reverse engineering platform
powered by rizin. It aims at being an advanced and customizable reverse engineering
platform while keeping the user experience in mind. Cutter is created by reverse
engineers for reverse engineers.")
    (license (list license:asl2.0
                   license:bsd-2
                   license:bsd-3
                   license:cc0
                   license:cc-by-sa3.0
                   license:cc-by-sa4.0
                   license:gpl1+
                   license:gpl2+
                   license:gpl3+
                   license:lgpl2.0+
                   license:lgpl2.1
                   license:lgpl2.1+
                   license:lgpl3
                   license:expat
                   license:ncsa))))
cutter-rizin
