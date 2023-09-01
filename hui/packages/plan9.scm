(define-module (hui packages nfs)
  #:use-module (gnu packages)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages perl)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu))

(define-public diod
  (let ((commit "9da28f911978957dbec251c653200db7a4dcad6e")
        (revision "0"))
    (package
      (name "diod")
      (version (git-version "1.0.23" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/chaos/diod")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1kf981m615w9x2l8km360ap80mlds7pgd44jgrblh87cq1aq8pms"))
         (patches
          (list
           (local-file "aux-files/diod/0001-not-a-git-repo.patch")))))
      (native-inputs (list automake autoconf m4 pkg-config libtool git-minimal
                           perl))
      (inputs (list popt ncurses lua-5.1 munge tcp-wrappers libcap attr))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-before 'configure 'fix-configure
             (lambda _
               (invoke "autoreconf" "-vif"))))))
      (home-page "https://github.com/chaos/diod")
      (synopsis "Distributed I/O Daemon")
      (description "a 9P file server")
      (license license:gpl2))))
