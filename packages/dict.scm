(use-modules
 (gnu)
 (gnu packages)
 (gnu packages compression)
 (guix packages)
 (guix build-system copy)
 (guix gexp)
 ((guix licenses) #:prefix license:)
 (guix utils)
 (guix download))

(define-public stardict-ecdict
  (package
    (name "stardict-ecdict")
    (version "2017")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/"
                                  "skywind3000/ECDICT/releases/download/"
                                  "1.0.28/ecdict-stardict-28.zip"))
              (sha256
               (base32
                "0b3gijq9qzyrp87i575gllyp0x70fhalmnk6cjwpkv6nvvrx01y7"))))
    (build-system copy-build-system)
    (native-inputs (list unzip))
    (arguments
     `(#:install-plan '(("stardict-ecdict-2.4.2.idx"
                         "share/stardict/stardict-ecdict-2.4.2.idx")
                        ("stardict-ecdict-2.4.2.ifo"
                         "share/stardict/stardict-ecdict-2.4.2.ifo")
                        ("stardict-ecdict-2.4.2.dict"
                         "share/stardict/stardict-ecdict-2.4.2.dict"))))
    (home-page "https://github.com/skywind3000/ECDICT")
    (synopsis "Free English to Chinese Dictionary Database")
    (description "Free English to Chinese Dictionary Database")
    (license license:expat)))
