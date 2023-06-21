(define-module (hui packages linux)
  #:use-module (gnu packages)
  #:use-module (gnu packages linux)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system linux-module)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix licenses:)
  #:use-module (guix download)
  #:use-module (srfi srfi-1))

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

;; not working, need pack thead xuantie toolchain
(define-public linux-lpi4a
  (let ((commit "7a1ebd4adb2fbf73db0c6d0ce114b2fff7a4f7bb")
        (revision "0"))
    (package
      (inherit linux-libre-riscv64-generic)
      (name "linux-lpi4a")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/revyos/thead-kernel")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0g0y3a5v0shilbs9274ryhqrxs8y42bbv7ygcnb927ysvqrd8bw7"))))
      (native-inputs
       `(("kconfig" ,(local-file "aux-files/lpi4a.config"))
         ,@(alist-delete "kconfig"
                         (package-native-inputs
                          linux-libre-riscv64-generic)))))))

linux-lpi4a
