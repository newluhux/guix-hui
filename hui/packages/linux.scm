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

(define-public linux-mi439-downstream-without-dtbs
  (package
    (inherit
     (customize-linux
      #:name "linux-mi439-downstream-without-dtbs"
      #:linux linux-libre-arm64-generic
      #:source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/LineageOS/android_kernel_xiaomi_sdm439")
              (commit "d17d32421fc95bff4df60a5c684e531295889ade")))
        (file-name (string-append "linux-mi439-git"))
        (sha256
         (base32
          "004jja6cb3655njh81dp71770djnxfyryallfwnrybknigbjynw8"))
        (patches
         (list
          (local-file "aux-files/mi439-kernel/0001-arch-arm64-configs-lineageos_mi439_defconfig-disable.patch")
          (local-file "aux-files/mi439-kernel/0002-arch-arm64-configs-lineageos_mi439_defconfig-remove-.patch")
          (local-file "aux-files/mi439-kernel/0003-drivers-bluetooth-btfm_slim.c-fix-include.patch")
          (local-file "aux-files/mi439-kernel/0004-techpack-audio-asoc-codecs-aw87519_audio.c-fix-inclu.patch")
          (local-file "aux-files/mi439-kernel/0005-drivers-gpu-msm-fix-include.patch")
          (local-file "aux-files/mi439-kernel/0006-drivers-media-platform-camera_v2-fix-build.patch")
          (local-file "aux-files/mi439-kernel/0007-include-trace-events-msm_cam.h-fix-include-drity-fix.patch")
          (local-file "aux-files/mi439-kernel/0008-drivers-platform-msm-ipa-ipa_clients-fix-include.patch")
          (local-file "aux-files/mi439-kernel/0009-drivers-usb-gadget-configfs-fix-include.patch")
          (local-file "aux-files/mi439-kernel/0010-Makefile-disable-techpack.patch"))))
      #:defconfig "lineageos_mi439_defconfig"
      #:extra-version "mi439"))
    (version "4.9.337")))

(define-public linux-mi439-downstream
  (let* ((kernel linux-mi439-downstream-without-dtbs))
    (package
      (inherit kernel)
      (name "linux-mi439-downstream")
      (arguments
       (substitute-keyword-arguments (package-arguments kernel)
         ((#:phases phases)
          #~(modify-phases #$phases
              (delete 'strip)
              (add-after 'install 'install-extran
                (lambda* (#:key outputs #:allow-other-keys)
                  (let* ((out (assoc-ref outputs "out"))
                         (dtbs (find-files "arch" "\\.dtb$"))
                         (dtbs-installdir (string-append out "/lib/dtbs"))
                         (kernels (find-files "." "^(Image.gz|Image.gz-dtb|vmlinux)$")))
                    (mkdir-p out)
                    (mkdir-p dtbs-installdir)
                    (for-each
                     (lambda (file)
                       (copy-file file (string-append dtbs-installdir "/" (basename file)))) dtbs)
                    (for-each
                     (lambda (file)
                       (copy-file
                        file (string-append out "/" (basename file)))) kernels)))))))))))

(define-public linux-allwinner-f1c100s
  (package
    (inherit
     (customize-linux
      #:name "linux-allwinner-f1c100s"
      #:linux linux-libre-arm-generic
      #:source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/torvalds/linux")
              (commit "a785fd28d31f76d50004712b6e0b409d5a8239d8")))
        (sha256
         (base32
          "15vxpgp40sf06gwl845jd25mpi6mxnj3fd5vkpm5pgs481b7s6xb"))
        (patches
         (list
          (local-file "aux-files/linux-f1c100s/0001-arch-arm-boot-dts-allwinner-suniv-f1c100s-licheepi-n.patch")
          (local-file "aux-files/linux-f1c100s/0002-arch-arm-configs-allwinner_f1c100s_defconfig-add-con.patch"))))
      #:defconfig "allwinner_f1c100s_defconfig"))
    (version "6.5.0-rc5")))

linux-allwinner-f1c100s
