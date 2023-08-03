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

(define-public linux-thead
  (let ((commit "8631d2c44f1160e75a940718c11d678b8e314710")
        (revision "0"))
    (package
      (inherit linux-libre-riscv64-generic)
      (name "linux-thead")
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
           "1nyrinvrgsnrry13qwx0mcxsii5m2qsi5kfyxmdvvdsvy4rjkdi4"))
         (patches
          (list
           (local-file
            "aux-files/thead-kernel/0001-drivers-soc-thead-fix-depend.patch")
	   (local-file
            "aux-files/thead-kernel/0002-arch-riscv-Kconfig.socs-select-THEAD-by-default.patch")))))
      (arguments
       (substitute-keyword-arguments (package-arguments linux-libre-riscv64-generic)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-before 'build 'use-revyos-defconfig
                (lambda _
                  (delete-file "arch/riscv/configs/defconfig")
                  (copy-file "arch/riscv/configs/revyos_defconfig"
                             "arch/riscv/configs/defconfig"))))))))))

linux-thead

