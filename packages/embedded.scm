;;; Copyright (C) 2023 Lu Hui <luhux76@gmail.com>

(use-modules (guix build-system gnu)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (guix utils))

(define-public gkermit
  (package
    (name "gkermit")
    (version "2.01")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/KermitProject/gkermit")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0njxzwnvanrdnx6cs7y0136g2179wx8wbqk8iqxpb48dr2hg7zn1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags (list (string-append "CC="
                                         ,(cc-for-target))
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-before 'build 'replace-hardcodepath
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin"))
                             (doc (string-append out "/share/doc"))
                             (man1 (string-append out "/share/man/man1"))
                             (info (string-append out "/share/info")))
                        (substitute* "makefile"
                          (("/usr/local/bin")
                           bin)
                          (("/usr/man/manl")
                           man1) ;may be a typo err?
                          (("/usr/local/doc")
                           doc)
                          (("/usr/local/info")
                           info)
                          (("CC=cc")
                           "CC ?= cc"))
                        (mkdir-p bin)
                        (mkdir-p doc)
                        (mkdir-p man1)
                        (mkdir-p info)))))))

    (home-page "https://github.com/KermitProject/gkermit")
    (synopsis "Kermit for UNIX")
    (description
     "Use for uploading and downloading files with Kermit protocol")
    (license license:gpl2))) ; note: some file not strict gpl2 license.

