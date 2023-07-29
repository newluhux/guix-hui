(define-module (hui packages nfs)
  #:use-module (gnu packages)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages onc-rpc)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public unfs3
  (package
    (name "unfs3")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/unfs3/unfs3/releases/download/"
                           "unfs3-" version "/unfs3-" version ".tar.gz"))
       (sha256
        (base32
         "1fwywxycral2m4yyb9jm3sf94hgcmw11h78z33ig6hrfpa7mdilv"))))
    (native-inputs (list flex bison pkg-config))
    (inputs (list libtirpc))
    (build-system gnu-build-system)
    (arguments `(#:tests? #f))
    (home-page "https://unfs3.github.io/")
    (synopsis "userspace nfs server impl")
    (description "It provides a daemon for the MOUNT and NFS protocols,
which are used by NFS clients for accessing files on the server.")
    (license license:expat)))

unfs3
