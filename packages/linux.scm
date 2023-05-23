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

(define-public linux-stable
  (package
    (inherit linux-libre-6.3)
    (name "linux-stable")
    (version "6.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://cdn.kernel.org/pub/linux/kernel/v6.x/linux-"
             version
             ".tar.xz"))
       (sha256
        (base32
         "1ra4kr9bp1s9d7amvz6ik1q3chwps5lysn37b28770pfdim22xc9"))))))

(define-public linux-firmware
  (package
    (name "linux-firmware")
    (version "20230404")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kernel.org/linux/kernel/firmware/"
                                  "linux-firmware-" version ".tar.xz"))
              (sha256
               (base32
                "01znf4gnymxn8q189gda6rlksw1nz1980ypkj0jcw71inlmsvyf3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags (list (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'validate-runpath))))
    (home-page "https://kernel.org")
    (synopsis "Nonfree firmware blobs for Linux")
    (description "nonfree linux kernel firmware")
    (license #f)))

(define-public rtl8821ce-linux-module
  (let ((commit "538c34671b391340e0ae23ff11bde77b6588496c")
        (revision "9"))
    (package
      (name "rtl8821ce-linux-module")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tomaspinho/rtl8821ce")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0p7xj032bp3h6wp27dhf2j42bgd4gvpk7w95n830awbj07c04dss"))))
      (build-system linux-module-build-system)
      (arguments
       (list #:make-flags
             #~(list (string-append "CC=" #$(cc-for-target))
                     (string-append "KSRC="
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
      (home-page "https://github.com/tomaspinho/rtl8821ce")
      (synopsis "Linux driver for Realtek RTL8821CE wireless network adapters")
      (description "This is Realtek's RTL8821CE Linux driver for wireless
network adapters.")
      (license licenses:gpl2))))

(define-public rtl8812au-aircrack-ng-linux-module
  (let ((commit "35308f4dd73e77fa572c48867cce737449dd8548")
        (revision "10"))
    (package
      (inherit rtl8821ce-linux-module)
      (name "rtl8812au-aircrack-ng-linux-module")
      (version (git-version "5.6.4.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/aircrack-ng/rtl8812au")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1clqrgmq5fhzybbiapmdbhg5qfx9k21r0hqa9pqmyinaqhvfnhfj"))
         (modules '((guix build utils)))
         (snippet
          #~(begin
              ;; Remove bundled tarballs, APKs, word lists, speadsheets,
              ;; and other unnecessary unlicenced things.
              (for-each delete-file-recursively (list "android"
                                                      "docs"
                                                      "tools"))))))
      (supported-systems '("x86_64-linux" "i686-linux"))
      (home-page "https://github.com/aircrack-ng/rtl8812au")
      (synopsis "Linux driver for Realtek USB wireless network adapters")
      (description
       "This is Realtek's rtl8812au Linux driver for USB 802.11n wireless
network adapters, modified by the aircrack-ng project to support monitor mode
and frame injection.  It provides a @code{88XXau} kernel module that supports
RTL8812AU, RTL8821AU, and RTL8814AU chips.")
      (license licenses:gpl2+))))

rtl8812au-aircrack-ng-linux-module
