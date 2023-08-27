(define-module (hui packages wallpaper)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download))

(define-public freedom-wallpaper
  (package
    (name "freedom-wallpaper")
    (version "2016")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.gnu.org/graphics/"
                    "this-is-freedom-wallpaper.png"))
              (sha256
               (base32
                "1hrr68zv7xmxdr9q8sz60kjiifiag5w633plmynwr2ydckx29lhx"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("this-is-freedom-wallpaper.png"
                         "share/wallpaper/this-is-freedom-wallpaper.png"))))
    (home-page "https://www.gnu.org/graphics/this-is-freedom-wallpaper.html")
    (synopsis "Freedom wallpaper")
    (description "Freedom wallpaper")
    (license license:gpl3)))

freedom-wallpaper
