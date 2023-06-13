(use-modules
 (gnu)
 (gnu packages)
 (gnu packages linux)
 (guix packages)
 (guix gexp)
 (guix utils)
 (guix build-system gnu)
 (guix build-system linux-module)
 (guix git-download)
 ((guix licenses) #:prefix licenses:)
 (guix download))

(define-public ch341-i2c-spi-gpio-linux-module
  (let ((commit "e90d2300535bcab9c26e938cf9dd4ca7672dfb0a")
        (revision "0"))
    (package
      (name "ch341-i2c-spi-gpio-linux-module")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/frank-zago/ch341-i2c-spi-gpio")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0k7z5yv8mjs7xpnm5g6pgkf01hd7ljybfqi2nkqigcbvlpyjaqxd"))))
      (build-system linux-module-build-system)
      (arguments
       (list #:make-flags
             #~(list (string-append "CC=" #$(cc-for-target))
                     (string-append "KDIR="
                                    (assoc-ref %build-inputs
                                               "linux-module-builder")
                                    "/lib/modules/build"))
             #:phases
             #~(modify-phases %standard-phases
                 (replace 'build
                   (lambda* (#:key (make-flags '()) (parallel-build? #t)
                             #:allow-other-keys)
                     (apply invoke "make"
                            `(,@(if parallel-build?
                                    `("-j" ,(number->string (parallel-job-count)))
                                    '())
                              ,@make-flags)))))
             #:tests? #f))              ; no test suite
      (home-page "https://github.com/frank-zago/ch341-i2c-spi-gpio")
      (synopsis "WinChipHead CH341 linux driver for I2C, SPI and GPIO mode  ")
      (description "The CH341 is declined in several flavors, and may support
one or more of UART, SPI, I2C and GPIO.")
      (license licenses:gpl2))))
