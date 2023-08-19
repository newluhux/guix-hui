;;; Copyright (C) 2023 Lu Hui <luhux76@gmail.com>

(define-module (hui packages embedded)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system cargo)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages busybox)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages base)
  #:use-module (gnu packages genimage)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages embedded)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages electronics)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages libftdi)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages python)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex))

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

(define-public colorcout
  (let ((commit "e64ffc5da038082c7cd35f4fce7b0580e2fe26fc")
	(revision "0"))
    (package
      (name "colorcout")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
	       (url "https://github.com/YuzukiTsuru/ColorCout")
	       (commit commit)))
         (file-name (git-file-name name version))
         (sha256
	  (base32 "0q5y1qwz3ykqa06jz0cqzw7r9h2fg6cwb1v711jj0x1c4hg19i8r"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f
	 #:phases
	 (modify-phases
	     %standard-phases
           (add-after 'unpack 'install-header
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((inc (string-append
                            (assoc-ref outputs "out") "/include")))
                 (mkdir-p inc)
                 (copy-file "includes/ColorCout.hpp"
                            (string-append inc "/ColorCout.hpp")))))
	   (replace 'install
	     (lambda* (#:key outputs #:allow-other-keys)
	       (let* ((bin (string-append (assoc-ref outputs "out") "/bin")))
		 (mkdir-p bin)
		 (copy-file "ColorBox" (string-append bin "/ColorBox"))))))))
      (home-page "https://github.com/YuzukiTsuru/ColorCout")
      (synopsis "Simple colored terminal text library in C++")
      (description "simple colored terminal text library in C++,
use ANSI color escape")
      (license license:wtfpl2))))

(define-public argparse
  (package
    (name "argparse")
    (version "2.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/p-ranav/argparse")
	     (commit (string-append "v" version))))
       (sha256
	(base32 "1wdpy45qcipfyw9bbr9s42v67b88bkyniy76yvh0grp2wf8zidxx"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/p-ranav/argparse")
    (synopsis "Argument Parser for C++ ")
    (description "single c++ header file for argument parse")
    (license license:expat)))

(define-public cpp-subprocess
  (let ((commit "af23f338801ed19696da42b1f9b97f8e21dec5d6")
	(revision "0"))
    (package
      (name "cpp-subprocess")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
	       (url "https://github.com/arun11299/cpp-subprocess")
	       (commit commit)))
         (file-name (git-file-name name version))
         (sha256
	  (base32 "0mbkqgxcckg6qb7b8qg33g6f8gpzamr2dkmm5zd3mjx9i4iqnzp9"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f))
      (home-page "https://github.com/arun11299/cpp-subprocess")
      (synopsis "Subprocessing with modern C++")
      (description "The only goal was to develop something that is as close as
python2.7 subprocess module in dealing with processes.")
      (license license:expat))))

(define-public inicpp
  (package
    (name "inicpp")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/SemaiCZE/inicpp")
	     (commit (string-append "v" version))))
       (sha256
	(base32 "0pi849rrs4py7kdimmasc0qp3vx9q2mdah3gdm5k8rin4jvvca54"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/SemaiCZE/inicpp")
    (synopsis "C++ parser of INI files with schema validation")
    (description "C++ parser of INI files with schema validation")
    (license license:expat)))

(define-public openixcard
  (let ((commit "07d9317d67975db9697a9699bc6be62c0361f11d")
	(revision "0"))
    (package
      (name "openixcard")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
	       (url "https://github.com/YuzukiTsuru/OpenixCard")
	       (commit commit)))
         (file-name (git-file-name name version))
         (sha256
	  (base32 "0q6zcd9jydh4fg0s4r7452x3rsajzah0dgr4qwznma4k0cjhxm3f"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f
         	 #:phases
	 (modify-phases
	     %standard-phases
           (add-after 'unpack 'disable-bundle
             (lambda _
               (substitute* "CMakeLists.txt"
                 (("add_subdirectory\\(lib/inicpp EXCLUDE_FROM_ALL\\)")
                  "")
                 (("add_subdirectory\\(lib/argparse EXCLUDE_FROM_ALL\\)")
                  "")
                 (("add_subdirectory\\(lib/ftxui EXCLUDE_FROM_ALL\\)")
                  ""))))
           (add-after 'unpack 'fix-include
             (lambda _
               (substitute* "src/GenIMG/GenIMG.cpp"
                 (("#include <subprocess.hpp>")
                  "#include <cpp-subprocess/subprocess.hpp>"))))
           (add-after 'unpack 'patch-shell
             (lambda _
               (substitute* "src/GenIMG/CMakeLists.txt"
                 (("./configure && make")
                  "sed -i -e 's/\\\\/bin\\\\/sh/bash/g' configure
 && ARFLAGS=\"\" sh configure
 && sed -i -e 's/\\(libgenimage_a_LIBADD\\)/\\(NOP\\)/g' Makefile
 && make --trace")))))))
      (native-inputs
       (list colorcout argparse cpp-subprocess ftxui inicpp
             autoconf automake m4 libtool pkg-config))
      (inputs
       (list libconfuse genimage))
      (home-page "https://github.com/YuzukiTsuru/OpenixCard")
      (synopsis "Open Source Version of Allwinner PhoenixCard")
      (description "Open Source Version of Allwinner PhoenixCard to
Dump, Unpack, Flash Allwinner IMG Files on Linux")
      (license license:gpl2))))

(define-public riscv64-unknown-elf-binutils
  (let ((xbinutils (cross-binutils "riscv64-unknown-elf")))
    (package
      (inherit xbinutils)
      (name "riscv64-unknown-elf-binutils")
      (arguments
       `(,@(substitute-keyword-arguments (package-arguments xbinutils)
             ((#:configure-flags flags)
              `(cons "--enable-multilib" ,flags))))))))

(define-public riscv64-unknown-elf-gcc
  (let ((xgcc (cross-gcc "riscv64-unknown-elf"
                         #:xbinutils riscv64-unknown-elf-binutils)))
    (package
      (inherit xgcc)
      (name "riscv64-unknown-elf-gcc")
      (arguments
       (substitute-keyword-arguments (package-arguments xgcc)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'set-paths 'augment-CPLUS_INCLUDE_PATH
                (lambda* (#:key inputs #:allow-other-keys)
                  (let ((gcc (assoc-ref inputs  "gcc")))
                    ;; Remove the default compiler from CPLUS_INCLUDE_PATH to
                    ;; prevent header conflict with the GCC from native-inputs.
                    (setenv "CPLUS_INCLUDE_PATH"
                            (string-join
                             (delete (string-append gcc "/include/c++")
                                     (string-split (getenv "CPLUS_INCLUDE_PATH")
                                                   #\:))
                             ":"))
                    (format #t
                            "environment variable `CPLUS_INCLUDE_PATH' changed to ~a~%"
                            (getenv "CPLUS_INCLUDE_PATH")))))
              (add-after 'unpack 'fix-genmultilib
                (lambda _
                  (substitute* "gcc/genmultilib"
                    (("#!/bin/sh") (string-append "#!" (which "sh"))))))))
         ((#:configure-flags flags)
          #~(append (list "--enable-multilib"
                          "--with-newlib"
                          "--with-host-libstdcxx=-static-libgcc -Wl,-Bstatic,-lstdc++,-Bdynamic -lm"
                          "--enable-plugins"
                          "--disable-decimal-float"
                          "--disable-libffi"
                          "--disable-libgomp"
                          "--disable-libmudflap"
                          "--disable-libquadmath"
                          "--disable-libssp"
                          "--disable-libstdcxx-pch"
                          "--disable-nls"
                          "--disable-shared"
                          "--disable-threads"
                          "--disable-tls")
                    (delete "--disable-multilib" #$flags)))))
      (native-search-paths
       (list (search-path-specification
              (variable "CROSS_C_INCLUDE_PATH")
              (files '("riscv64-unknown-elf/include")))
             (search-path-specification
              (variable "CROSS_CPLUS_INCLUDE_PATH")
              (files '("riscv64-unknown-elf/include"
                       "riscv64-unknown-elf/include/c++"
                       "riscv64-unknown-elf/include/c++/riscv64-unknown-elf")))
             (search-path-specification
              (variable "CROSS_LIBRARY_PATH")
              (files '("riscv64-unknown-elf/lib"))))))))

(define-public riscv64-unknown-elf-newlib
  (let ((commit "e301a74a6f111df4553b50b813a589589d1708b1")
        (revision "0"))
    (package
      (inherit newlib-arm-none-eabi)
      (name "riscv64-unknown-elf-newlib")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "http://sourceware.org/git/newlib-cygwin.git")
               (commit commit)))
         (file-name (git-file-name "newlib" commit))
         (sha256
          (base32
           "0ylibsavxqbwnl6yqxql3j2dzax843wahy93fncqlnmm3cfnz9i0"))))
      (arguments
       `(#:out-of-source? #t
         #:configure-flags '("--target=riscv64-unknown-elf"
                             "--enable-multilib"
                             "--enable-newlib-io-long-long"
                             "--enable-newlib-io-c99-formats"
                             "--enable-newlib-register-fini"
                             "--enable-newlib-retargetable-locking"
                             "--disable-newlib-supplied-syscalls"
                             "--disable-nls")))
      (native-inputs
       `(("xbinutils" ,(cross-binutils "riscv64-unknown-elf"))
         ("xgcc" ,riscv64-unknown-elf-gcc)
         ("texinfo" ,texinfo))))))

(define-public riscv64-unknown-elf-newlib-nano
  (package (inherit riscv64-unknown-elf-newlib)
    (name "riscv64-unknown-elf-newlib-nano")
    (arguments
     `(#:out-of-source? #t
       #:configure-flags
       '("--target=riscv64-unknown-elf"
         "--enable-multilib"
         "--disable-newlib-supplied-syscalls"
         "--enable-newlib-reent-small"
         "--disable-newlib-fvwrite-in-streamio"
         "--disable-newlib-fseek-optimization"
         "--disable-newlib-wide-orient"
         "--enable-newlib-nano-malloc"
         "--disable-newlib-unbuf-stream-opt"
         "--enable-lite-exit"
         "--enable-newlib-global-atexit"
         "--enable-newlib-nano-formatted-io"
         "--disable-nls")
       #:phases
       (modify-phases %standard-phases
         (delete 'strip)
         (add-after 'install 'hardlink-newlib
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; The nano.specs file says that newlib-nano files should end
               ;; in "_nano.a" instead of just ".a".  Note that this applies
               ;; to all the multilib folders too.
               (for-each
                (lambda (file)
                  (link
                   file
                   (string-append
                    ;; Strip ".a" off the end
                    (substring file 0 (- (string-length file) 2))
                    ;; Add "_nano.a" onto the end
                    "_nano.a")))
                (find-files
                 out
                 "^(libc.a|libm.a|libg.a|libgloss.a|libsemihost.a|libnosys.a|librdimon.a|libstdc\\+\\+.a|libsupc\\+\\+.a)$"))
               (mkdir-p (string-append out "/riscv64-unknown-elf/include/newlib-nano"))
               (symlink
                "../newlib.h"
                (string-append out "/riscv64-unknown-elf/include/newlib-nano/newlib.h"))
               #t))))))
    (synopsis "Newlib variant for small systems with limited memory")))

(define-public riscv64-unknown-elf-gdb
  (package
    (inherit gdb)
    (name "riscv64-unknown-elf-gdb")
    (arguments
     `(#:configure-flags '("--target=riscv64-unknown-elf"
                           "--enable-multilib"
                           "--enable-interwork"
                           "--enable-languages=c,c++"
                           "--disable-nls")
       ,@(package-arguments gdb)))))

;; test pass on: https://github.com/sipeed/RV-Debugger-BL702
;; :)
(define-public riscv64-unknown-elf-toolchain
  (package
    (name "riscv64-unknown-elf-toolchain")
    (version (package-version riscv64-unknown-elf-gcc))
    (source #f)
    (build-system trivial-build-system)
    (propagated-inputs
     (list riscv64-unknown-elf-binutils riscv64-unknown-elf-gcc
           riscv64-unknown-elf-newlib riscv64-unknown-elf-newlib-nano))
    (arguments
     '(#:modules ((guix build union))
       #:builder
       (begin
         (use-modules (ice-9 match)
                      (guix build union))
         (match %build-inputs
           (((names . directories) ...)
            (union-build (assoc-ref %outputs "out")
                         directories)
            #t)))))
    (home-page (package-home-page gcc-toolchain))
    (synopsis (package-synopsis gcc-toolchain))
    (description (package-description gcc-toolchain))
    (license (package-license gcc-toolchain))))

(define-public argtable3
  (package
    (name "argtable3")
    (version "3.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/argtable/argtable3")
                    (commit "v3.2.2.f25c624")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zkivsaxqh5gbmvjbz0x8jxf1x4n574wpw1wiwhd3s7cn0a73ksz"))))
    (build-system cmake-build-system)
    (home-page "https://www.argtable.org/")
    (synopsis "cross platform single file ansi c command line parsing library")
    (description "Argtable is an open source ANSI C library that parses
GNU-style command-line options")
    (license license:bsd-3)))

(define-public blisp
  (package
    (name "blisp")
    (version "0.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pine64/blisp")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0s9asydsm97vi001740wf3pl1q8k8a06vw8knz3b40ahdifrpb69"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("libserialport-src"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/sigrokproject/libserialport/")
                 (commit "6f9b03e597ea7200eb616a4e410add3dd1690cb1")))
           (file-name "libserialport-src")
           (sha256
            (base32
             "1q3is08k6jwqlhwsw8krrbddhzdkr5x5s1hxcx1f38ij5h256l53"))))
       ("argtable3-src"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/argtable/argtable3")
                 (commit "6f0e40bc44c99af353ced367c6fafca8705f5fca")))
           (file-name "argtable3-src")
           (sha256
            (base32
             "0rnam9km53v3wfgdfp9nz67a55ng2sv1ylnyy2dnjy04m3649x19"))))))
    (arguments
     `(#:tests? #f
       #:configure-flags '("-DBLISP_BUILD_CLI=ON")
       #:phases
       (modify-phases
	   %standard-phases
         (add-after 'unpack 'install-bundle
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((libserialport (assoc-ref inputs "libserialport-src"))
                    (argtable3 (assoc-ref inputs "argtable3-src")))
               (delete-file-recursively "vendor/libserialport")
               (copy-recursively libserialport "vendor/libserialport")
               (delete-file-recursively "vendor/argtable3")
               (copy-recursively argtable3 "vendor/argtable3"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib")))
               (mkdir-p bin)
               (copy-file "tools/blisp/blisp" (string-append bin "/blisp"))
               (copy-recursively "shared" lib)
               (copy-file "static/libblisp.a" (string-append lib "/libblisp.a"))))))))
    (home-page "https://github.com/pine64/blisp")
    (synopsis "ISP tool & library for Bouffalo Labs RISC-V
Microcontrollers and SoCs ")
    (description "Bouffalo Labs ISP (in-system-programming) tool & library:
an open source tool to flash Bouffalo RISC-V MCUs.")
    (license license:expat)))

(define-public python-extract-dtb
  (package
    (name "python-extract-dtb")
    (version "1.2.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "extract-dtb" version))
              (sha256
               (base32
                "1a7rfvwisgri8b00pch6d9pfrl8s93w8g09yzxf4xh0qvmsxmh43"))))
    (build-system python-build-system)
    (home-page "https://github.com/PabloCastellano/extract-dtb/")
    (synopsis "Extract device tree blobs (dtb) from kernel images")
    (description "Tool to split a kernel image with appended dtbs into
separated kernel and dtb files.")
    (license license:gpl3+)))

(define-public ek
  (package
    (name "ek")
    (version "1.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.kermitproject.org/ftp/kermit/archives/ek18.tar"))
              (sha256
               (base32
                "06yslag2hckm6xd6g0yvhv78p4qqsmf91fgvqgi7ly85xqzlziar"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags (list (string-append "CC="
                                         ,(cc-for-target))
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin")))
                        (mkdir-p bin)
                        (copy-file "ek" (string-append bin "/ek"))))))))
    (home-page "https://kermitproject.org/ek.html")
    (synopsis "Compact, fast, robust, portable Kermit file transfer source code to embed in devices or C programs")
    (description "EK (Embedded Kermit, E-Kermit) is an implementation of the Kermit file transfer protocol
written in ANSI C and designed for embedding in devices or firmware, for use in realtime applications,
or for construction of DLLs and libraries.")
    (license license:bsd-3)))

(define-public eksw
  (package
    (name "eksw")
    (version "0.94")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://kermitproject.org/ftp/kermit/archives/eksw094.zip"))
              (sha256
               (base32
                "0hb8ipbcvrl4jys0hph7yf5ayn33gal13isksz8ycxnjfgf58591"))))
    (build-system gnu-build-system)
    (native-inputs (list unzip))
    (arguments
     `(#:tests? #f
       #:make-flags (list (string-append "CC="
                                         ,(cc-for-target))
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin")))
                        (mkdir-p bin)
                        (copy-file "eksw" (string-append bin "/eksw"))))))))
    (home-page "https://kermitproject.org/ek.html")
    (synopsis "Compact, fast, robust, portable Kermit file transfer source code to embed in devices or C programs")
    (description "EKSW is a new version of E-Kermit that includes true sliding windows packet transport.")
    (license license:bsd-3)))

(define-public ckermit
  (let ((commit "64ae06b6eab943b78b75de8e65282017fcf9b759")
        (revision "0"))
      (package
    (name "ckermit")
    (version (git-version "0" revision commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "https://github.com/KermitProject/ckermit")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32 "00xnavdvpjq4w34a518cp2jm0h3na4a0zsdld41sg5hj7qx52ixi"))))
    (build-system gnu-build-system)
    (inputs (list ncurses))
    (arguments
     `(#:tests? #f
       #:make-flags (list (string-append "CC="
                                         ,(cc-for-target))
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (add-after 'unpack 'fix-build
                    (lambda _
                      (substitute* "ckucmd.c"
                        (("IO_file_flags") "IO_EOF_SEEN"))))
                  (replace 'build
                    (lambda* (#:key make-flags #:allow-other-keys)
                      (apply invoke "make" "linux" "LNKFLAGS=-lcrypt" "KFLAGS=-DOPENSSL_097" make-flags)))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin"))
                             (man1 (string-append out "/man/man1")))
                        (mkdir-p bin)
                        (copy-file "wermit" (string-append bin "/ckermit"))
                        (mkdir-p man1)
                        (copy-file "ckuker.nr" (string-append man1 "/ckermit.1"))))))))
    (home-page "https://www.kermitproject.org/ckermit.html")
    (synopsis "Portable scriptable network and serial communication software")
    (description "C-Kermit is a combined network and serial communication software package offering a consistent,
 transport-independent, cross-platform approach to connection establishment, terminal sessions, file transfer,
file management, character-set translation, numeric and alphanumeric paging, and automation of file transfer
and management, dialogs, and communication tasks through its built-in scripting language.")
    (license #f)))) ; custom license

(define-public ukermit
  (let ((commit "7c9100a3471ad98bd9c8c876be716e91554400a7")
	(revision "0"))
    (package
      (name "ukermit")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
	       (url "https://github.com/newluhux/ukermit")
	       (commit commit)))
         (file-name (git-file-name name version))
         (sha256
	  (base32 "1fvxm10vgyl23phlfng2jpvrxyfzvjgs52gs8ldcmdwri63kpw0f"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f
	 #:make-flags (list (string-append "CC="
                                          ,(cc-for-target))
                            (string-append "PREFIX="
                                           (assoc-ref %outputs "out")))
	 #:phases
	 (modify-phases
	  %standard-phases
	  (delete 'configure)
	  (replace
	   'install
	   (lambda* (#:key outputs #:allow-other-keys)
		    (let* ((out (assoc-ref outputs "out"))
			   (bin (string-append out "/bin")))
		      (mkdir-p bin)
		      (copy-file "ukermit" (string-append bin "/ukermit"))))))))
      (home-page "https://github.com/newluhux/ukermit")
      (synopsis "Simple kermit download program")
      (description "Simple kermit download program, support 7bit prefix.")
      (license license:expat))))

(define-public libsigrok-next
  (let ((commit "0db1b189bee3ffe5c6ea39d7ca2e62715856b538")
	(revision "0"))
    (package
     (name "libsigrok-next")
     (version (git-version "0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "git://sigrok.org/libsigrok")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32 "0mpnh28gsnd8z205sikc7is6kxwx180jrdkxxxfj6lm8rzb2ldah"))))
     (build-system gnu-build-system)
     (native-inputs (list autoconf automake m4 pkg-config
			  libtool doxygen))
     (inputs (list glib libzip libserialport libusb zlib libftdi
		   nettle hidapi glibmm python bluez))
     (arguments
      `(#:phases
	(modify-phases
	 %standard-phases
         (add-after 'install 'install-udev-rules
		    (lambda*
		     (#:key outputs #:allow-other-keys)
		     (let* ((udev (string-append
				   (assoc-ref outputs "out") "/lib/udev/rules.d")))
		       (mkdir-p udev)
                       (substitute* "contrib/61-libsigrok-plugdev.rules"
                         (("plugdev") "dialout"))
		       (copy-file "./contrib/60-libsigrok.rules"
				  (string-append udev "/60-sigrok.rules"))
		       (copy-file "./contrib/61-libsigrok-plugdev.rules"
				  (string-append udev "/61-libsigrok-plugdev.rules"))
		       (copy-file "./contrib/61-libsigrok-uaccess.rules"
				  (string-append udev "/61-libsigrok-uaccess.rules"))))))))
     (home-page "http://sigrok.org/wiki/Libsigrok")
     (synopsis "logic analyzer library")
     (description "shared library written in C,
hich provides the basic hardware access drivers for logic analyzers and
other supported devices, as well as input/output file format support.")
     (license license:gpl3))))

(define-public libsigrokdecode-next
  (let ((commit "73cb5461acdbd007f4aa9e81385940fad6607696")
	(revision "0"))
    (package
     (name "libsigrokdecode-next")
     (version (git-version "0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "git://sigrok.org/libsigrokdecode")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32 "1i4jpkhb9yqf1fmbvlzdifj9arkspspffn93q8yh4vq5zr27k085"))))
     (build-system gnu-build-system)
     (native-inputs (list autoconf automake m4 pkg-config
			  libtool doxygen))
     (inputs (list glib python))
     (home-page "http://sigrok.org/wiki/Libsigrokdecode")
     (synopsis "provides (streaming) protocol decoding functionality.")
     (description "libsigrokdecode is a shared library which provides
(streaming) protocol decoding functionality. ")
     (license license:gpl3))))

(define-public pluseview-next
  (let ((commit "136995b831c50d3261143b1183c73af55c9ba3a5")
	(revision "0"))
    (package
     (name "pluseview-next")
     (version (git-version "0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "git://sigrok.org/pluseview")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32 "1m3cmp421qzgyj8bv13s4zraczjkrvwy58nxpwcwzb3c4w08919q"))))
     (build-system cmake-build-system)
     (native-inputs (list pkg-config libtool))
     (inputs (list boost glib libsigrok-next libserialport libftdi hidapi
                   qtbase-5 bluez nettle glibmm libzip libsigrokdecode-next
                   python qtsvg-5 qttools-5))
     (arguments `(#:tests? #f))
     (home-page "http://sigrok.org/wiki/PulseView")
     (synopsis "Qt based logic analyzer, oscilloscope and MSO GUI for sigrok.")
     (description "It can acquire samples from a supported device and display
them, load and display captures from existing sigrok *.sr files, as well as run
protocol decoders and display their annotations. ")
     (license license:gpl3))))

(define-public sigrok-cli-next
  (let ((commit "394fd9b7a456f16c7ac15f41b0e29081f1d951f8")
	(revision "0"))
    (package
     (name "sigrok-cli-next")
     (version (git-version "0" revision commit))
     (source
      (origin
       (method git-fetch)
       (uri (git-reference
	     (url "git://sigrok.org/sigrok-cli")
	     (commit commit)))
       (file-name (git-file-name name version))
       (sha256
	(base32 "06m5c5h9j1ng6w9il1qcyaf3mq7cj80s65s66pnhc0lipzqmawqf"))))
     (build-system gnu-build-system)
     (native-inputs (list autoconf automake m4 pkg-config
			  libtool))
     (inputs (list glib libsigrok libsigrokdecode libserialport
		   libftdi hidapi bluez nettle libzip))
     (home-page "http://sigrok.org/wiki/Sigrok-cli")
     (synopsis "a command-line frontend for sigrok. ")
     (description "It supports sample acquisition from logic analyzer,
oscilloscope, multimeter, and other hardware, as well as running protocol
decoders over the sample data (either from hardware or loaded from files). ")
     (license license:gpl3))))

(define-public rust-syn-2
  (package
    (name "rust-syn")
    (version "2.0.27")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "syn" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1q25pa0d0i66dnq5b9c1vbr3bm4nlmkayzb5ijf5n9d88hznf3xn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-unicode-ident" ,rust-unicode-ident-1))))
    (home-page "https://github.com/dtolnay/syn")
    (synopsis "Parser for Rust source code")
    (description "Parser for Rust source code")
    (license (list license:expat license:asl2.0))))

(define-public rust-quote-1
  (package
    (name "rust-quote")
    (version "1.0.32")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "quote" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0rarx33n4sp7ihsiasrjip5qxh01f5sn80daxc6m885pryfb7wsh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1))))
    (home-page "https://github.com/dtolnay/quote")
    (synopsis "Quasi-quoting macro quote!(...)")
    (description "Quasi-quoting macro quote!(...)")
    (license (list license:expat license:asl2.0))))

(define-public rust-proc-macro2-1
  (package
    (name "rust-proc-macro2")
    (version "1.0.66")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "proc-macro2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ngawak3lh5p63k5x2wk37qy65q1yylk1phwhbmb5pcv7zdk3yqq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-unicode-ident" ,rust-unicode-ident-1))))
    (home-page "https://github.com/dtolnay/proc-macro2")
    (synopsis
     "A substitute implementation of the compiler's `proc_macro` API to decouple token-based libraries from the procedural macro use case.")
    (description
     "This package provides a substitute implementation of the compiler's `proc_macro`
API to decouple token-based libraries from the procedural macro use case.")
    (license (list license:expat license:asl2.0))))

(define-public rust-thiserror-impl-1
  (package
    (name "rust-thiserror-impl")
    (version "1.0.44")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "thiserror-impl" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "15nwh6qfwxlwimgij1p6ajb377p4rlvvc6sx7amiz11h959rh089"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/dtolnay/thiserror")
    (synopsis "Implementation detail of the `thiserror` crate")
    (description "Implementation detail of the `thiserror` crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-thiserror-1
  (package
    (name "rust-thiserror")
    (version "1.0.44")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "thiserror" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "143zzmardcq447va2pw09iq9rajvr48v340riljghf84iah40431"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-thiserror-impl" ,rust-thiserror-impl-1))))
    (home-page "https://github.com/dtolnay/thiserror")
    (synopsis "derive(Error)")
    (description "derive(Error)")
    (license (list license:expat license:asl2.0))))

(define-public rust-simplelog-0.12
  (package
    (name "rust-simplelog")
    (version "0.12.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "simplelog" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0sa3hjdifxhcb9lnlg549fr2cc7vz89nygwbih2dbqsx3h20ivmc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ansi-term" ,rust-ansi-term-0.12)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-paris" ,rust-paris-1)
                       ("rust-termcolor" ,rust-termcolor-1)
                       ("rust-time" ,rust-time-0.3))))
    (home-page "https://github.com/drakulix/simplelog.rs")
    (synopsis "A simple and easy-to-use logging facility for Rust's log crate")
    (description
     "This package provides a simple and easy-to-use logging facility for Rust's log
crate")
    (license (list license:expat license:asl2.0))))

(define-public rust-libusb1-sys-0.6
  (package
    (name "rust-libusb1-sys")
    (version "0.6.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "libusb1-sys" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "09sznaf1lkahb6rfz2j0zbrcm2viz1d1wl8qlk4z4ia2rspy5l7r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-cc" ,rust-cc-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-pkg-config" ,rust-pkg-config-0.3)
                       ("rust-vcpkg" ,rust-vcpkg-0.2))))
    (home-page "https://github.com/a1ien/rusb")
    (synopsis "FFI bindings for libusb.")
    (description "FFI bindings for libusb.")
    (license license:expat)))

(define-public rust-rusb-0.9
  (package
    (name "rust-rusb")
    (version "0.9.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rusb" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1lf5hpvka5rr19bpww3mk8gi75xkr54gl79cf6za7cgr2ilw7a24"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-libc" ,rust-libc-0.2)
                       ("rust-libusb1-sys" ,rust-libusb1-sys-0.6))))
    (home-page "https://github.com/a1ien/rusb")
    (synopsis "Rust library for accessing USB devices.")
    (description "Rust library for accessing USB devices.")
    (license license:expat)))

(define-public rust-wasmparser-0.102
  (package
    (name "rust-wasmparser")
    (version "0.102.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wasmparser" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0jqh6p7w5kng9vz1k1bblwfx6l4fbnqr2sxgksmik0jrszils4s8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-indexmap" ,rust-indexmap-1)
                       ("rust-url" ,rust-url-2))))
    (home-page
     "https://github.com/bytecodealliance/wasm-tools/tree/main/crates/wasmparser")
    (synopsis
     "A simple event-driven library for parsing WebAssembly binary files.
")
    (description
     "This package provides a simple event-driven library for parsing
@code{WebAssembly} binary files.")
    (license (list license:asl2.0))))

(define-public rust-ruzstd-0.3
  (package
    (name "rust-ruzstd")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ruzstd" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1zkz8s4ws2h1ccmjrlm22v0hm31klqimvzll6hgw5npry1hyc5cs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-twox-hash" ,rust-twox-hash-1))))
    (home-page "https://github.com/KillingSpark/zstd-rs")
    (synopsis "A decoder for the zstd compression format")
    (description
     "This package provides a decoder for the zstd compression format")
    (license license:expat)))

(define-public rust-hashbrown-0.13
  (package
    (name "rust-hashbrown")
    (version "0.13.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "hashbrown" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "03ji3n19j4b6mf2wlla81vsixcmlivglp6hgk79d1pcxfcrw38s3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ahash" ,rust-ahash-0.8)
                       ("rust-bumpalo" ,rust-bumpalo-3)
                       ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/rust-lang/hashbrown")
    (synopsis "A Rust port of Google's SwissTable hash map")
    (description
     "This package provides a Rust port of Google's @code{SwissTable} hash map")
    (license (list license:expat license:asl2.0))))

(define-public rust-object-0.31
  (package
    (name "rust-object")
    (version "0.31.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "object" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1lb670wallm2i6rzrx2hz1afya4bfjzz6n9zhfw52l1bkxyndnlb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-crc32fast" ,rust-crc32fast-1)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-hashbrown" ,rust-hashbrown-0.13)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-ruzstd" ,rust-ruzstd-0.3)
                       ("rust-wasmparser" ,rust-wasmparser-0.102))))
    (home-page "https://github.com/gimli-rs/object")
    (synopsis
     "A unified interface for reading and writing object file formats.")
    (description
     "This package provides a unified interface for reading and writing object file
formats.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-nu-ansi-term-0.47
  (package
    (name "rust-nu-ansi-term")
    (version "0.47.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "nu-ansi-term" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0xmghm1knrk3j4f79cwhgb0vci38fxrk3gg9cb1399mw2zhk3w0x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1)
                       ("rust-windows-sys" ,rust-windows-sys-0.45))))
    (home-page "https://github.com/nushell/nu-ansi-term")
    (synopsis "Library for ANSI terminal colors and styles (bold, underline)")
    (description
     "Library for ANSI terminal colors and styles (bold, underline)")
    (license license:expat)))

(define-public rust-nu-pretty-hex-0.81
  (package
    (name "rust-nu-pretty-hex")
    (version "0.81.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "nu-pretty-hex" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1vwwvszry4gq8r1yiyj02mlypparq3blw55zwyyg7b4lwx1bnmfc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-nu-ansi-term" ,rust-nu-ansi-term-0.47))))
    (home-page
     "https://github.com/nushell/nushell/tree/main/crates/nu-pretty-hex")
    (synopsis "Pretty hex dump of bytes slice in the common style.")
    (description "Pretty hex dump of bytes slice in the common style.")
    (license license:expat)))

(define-public rust-ihex-3
  (package
    (name "rust-ihex")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "ihex" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1wlzfyy5fsqgpki5vdapw0jjczqdm6813fgd3661wf5vfi3phnin"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "http://github.com/martinmroz/ihex")
    (synopsis
     "A Rust library for parsing and generating Intel HEX (or IHEX) objects. This format is commonly used for representing compiled program code and data to be loaded into a microcontroller, flash memory or ROM.")
    (description
     "This package provides a Rust library for parsing and generating Intel HEX (or
IHEX) objects.  This format is commonly used for representing compiled program
code and data to be loaded into a microcontroller, flash memory or ROM.")
    (license (list license:expat license:asl2.0))))

(define-public rust-bitfield-0.14
  (package
    (name "rust-bitfield")
    (version "0.14.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "bitfield" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1b26k9acwss4qvrl60lf9n83l17d4hj47n5rmpd3iigf9j9n0zid"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/dzamlo/rust-bitfield")
    (synopsis "This crate provides macros to generate bitfield-like struct.")
    (description
     "This crate provides macros to generate bitfield-like struct.")
    (license (list license:expat license:asl2.0))))

(define-public rust-wlink-0.0.4
  (package
    (name "rust-wlink")
    (version "0.0.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wlink" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1q1z69dmvjb3xxb3m23v17bymaz4nqh1xjmf846g3xh45l59xp1d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-bitfield" ,rust-bitfield-0.14)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-ihex" ,rust-ihex-3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-nu-pretty-hex" ,rust-nu-pretty-hex-0.81)
                       ("rust-object" ,rust-object-0.31)
                       ("rust-rusb" ,rust-rusb-0.9)
                       ("rust-simplelog" ,rust-simplelog-0.12)
                       ("rust-thiserror" ,rust-thiserror-1))))
    (home-page "https://github.com/ch32-rs/wlink")
    (synopsis
     "WCH-Link flash tool for WCH's RISC-V MCUs(CH32V, CH56X, CH57X, CH58X)")
    (description
     "WCH-Link flash tool for WCH's RISC-V MCUs(CH32V, CH56X, CH57X, CH58X)")
    (license (list license:expat license:asl2.0))))

(define-public rust-unsafe-libyaml-0.2
  (package
    (name "rust-unsafe-libyaml")
    (version "0.2.9")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "unsafe-libyaml" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1yjwnz124wp1fhj075rdqkz00n2gahzj9yi5ixnmiinkw79ng17j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/dtolnay/unsafe-libyaml")
    (synopsis "libyaml transpiled to rust by c2rust")
    (description "libyaml transpiled to rust by c2rust")
    (license license:expat)))

(define-public rust-rustc-rayon-core-0.5
  (package
    (name "rust-rustc-rayon-core")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rustc-rayon-core" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0zqbr87x58j2g9rgm2lc0254b6yqabb41jvddw99qd8fy2m8srk7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-crossbeam-channel" ,rust-crossbeam-channel-0.5)
                       ("rust-crossbeam-deque" ,rust-crossbeam-deque-0.8)
                       ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.8)
                       ("rust-num-cpus" ,rust-num-cpus-1))))
    (home-page "https://github.com/rust-lang/rustc-rayon")
    (synopsis "Core APIs for Rayon - fork for rustc")
    (description "Core APIs for Rayon - fork for rustc")
    (license (list license:expat license:asl2.0))))

(define-public rust-rustc-rayon-0.5
  (package
    (name "rust-rustc-rayon")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rustc-rayon" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "040p2am25g3isnpsixrcrjrv70yz2lzkbq8gpv76xjipi3fam0gb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-either" ,rust-either-1)
                       ("rust-rustc-rayon-core" ,rust-rustc-rayon-core-0.5))))
    (home-page "https://github.com/rust-lang/rustc-rayon")
    (synopsis "Simple work-stealing parallelism for Rust - fork for rustc")
    (description "Simple work-stealing parallelism for Rust - fork for rustc")
    (license (list license:expat license:asl2.0))))

(define-public rust-rkyv-derive-0.7
  (package
    (name "rust-rkyv-derive")
    (version "0.7.42")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rkyv_derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "07alynj16yqlyprlwqd8av157rrywvid2dm7swbhl8swbf8npq5j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/rkyv/rkyv")
    (synopsis "Derive macro for rkyv")
    (description "Derive macro for rkyv")
    (license license:expat)))

(define-public rust-bytecheck-derive-0.6
  (package
    (name "rust-bytecheck-derive")
    (version "0.6.11")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "bytecheck_derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0qdgfqx23gbjp5scbc8fhqc5kd014bpxn8hc9i9ssd8r4rplrv57"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/djkoloski/bytecheck")
    (synopsis "Derive macro for bytecheck")
    (description "Derive macro for bytecheck")
    (license license:expat)))

(define-public rust-bytecheck-0.6
  (package
    (name "rust-bytecheck")
    (version "0.6.11")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "bytecheck" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "09xnpjfhw36a973dpdd2mcmb93rrix539j49vkkgcqf878174qwb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-bytecheck-derive" ,rust-bytecheck-derive-0.6)
                       ("rust-ptr-meta" ,rust-ptr-meta-0.1)
                       ("rust-simdutf8" ,rust-simdutf8-0.1)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/djkoloski/bytecheck")
    (synopsis "Derive macro for bytecheck")
    (description "Derive macro for bytecheck")
    (license license:expat)))

(define-public rust-rkyv-0.7
  (package
    (name "rust-rkyv")
    (version "0.7.42")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "rkyv" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0n2wzwnghkr2ny16c08f5szbkljfqrp3s8fnnb096f011ciwh002"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arrayvec" ,rust-arrayvec-0.7)
                       ("rust-bitvec" ,rust-bitvec-1)
                       ("rust-bytecheck" ,rust-bytecheck-0.6)
                       ("rust-hashbrown" ,rust-hashbrown-0.12)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-ptr-meta" ,rust-ptr-meta-0.1)
                       ("rust-rend" ,rust-rend-0.4)
                       ("rust-rkyv-derive" ,rust-rkyv-derive-0.7)
                       ("rust-seahash" ,rust-seahash-4)
                       ("rust-smallvec" ,rust-smallvec-1)
                       ("rust-tinyvec" ,rust-tinyvec-1)
                       ("rust-uuid" ,rust-uuid-1))))
    (home-page "https://github.com/rkyv/rkyv")
    (synopsis "Zero-copy deserialization framework for Rust")
    (description "Zero-copy deserialization framework for Rust")
    (license license:expat)))

(define-public rust-allocator-api2-0.2
  (package
    (name "rust-allocator-api2")
    (version "0.2.16")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "allocator-api2" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1iayppgq4wqbfbfcqmsbwgamj0s65012sskfvyx07pxavk3gyhh9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/zakarumych/allocator-api2")
    (synopsis "Mirror of Rust's allocator API")
    (description "Mirror of Rust's allocator API")
    (license (list license:expat license:asl2.0))))

(define-public rust-hashbrown-0.14
  (package
    (name "rust-hashbrown")
    (version "0.14.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "hashbrown" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0yj3nf0w30pf30w503kgaw4sbjnh62l5cbmc7dd0mnczzywh2qic"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-ahash" ,rust-ahash-0.8)
                       ("rust-allocator-api2" ,rust-allocator-api2-0.2)
                       ("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-rkyv" ,rust-rkyv-0.7)
                       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/rust-lang/hashbrown")
    (synopsis "A Rust port of Google's SwissTable hash map")
    (description
     "This package provides a Rust port of Google's @code{SwissTable} hash map")
    (license (list license:expat license:asl2.0))))

(define-public rust-equivalent-1
  (package
    (name "rust-equivalent")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "equivalent" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1malmx5f4lkfvqasz319lq6gb3ddg19yzf9s8cykfsgzdmyq0hsl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/cuviper/equivalent")
    (synopsis "Traits for key comparison in maps.")
    (description "Traits for key comparison in maps.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-indexmap-2
  (package
    (name "rust-indexmap")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "indexmap" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0pdnbvv6gnyxx2li8mks8p00fya3ynmhx3n6infpcy8a4gi7yiym"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-equivalent" ,rust-equivalent-1)
                       ("rust-hashbrown" ,rust-hashbrown-0.14)
                       ("rust-quickcheck" ,rust-quickcheck-1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-rustc-rayon" ,rust-rustc-rayon-0.5)
                       ("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/bluss/indexmap")
    (synopsis "A hash table with consistent order and fast iteration.")
    (description
     "This package provides a hash table with consistent order and fast iteration.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-serde-yaml-0.9
  (package
    (name "rust-serde-yaml")
    (version "0.9.25")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "serde_yaml" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0x7mj753makrxxn9k7lj84zgvymhq7mqrkfhc75labs5wiwf2j8s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-indexmap" ,rust-indexmap-2)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-ryu" ,rust-ryu-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-unsafe-libyaml" ,rust-unsafe-libyaml-0.2))))
    (home-page "https://github.com/dtolnay/serde-yaml")
    (synopsis "YAML data format for Serde")
    (description "YAML data format for Serde")
    (license (list license:expat license:asl2.0))))

(define-public rust-scroll-derive-0.11
  (package
    (name "rust-scroll-derive")
    (version "0.11.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "scroll_derive" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1bi5ljnzksvqhic6j7i2a2ap41s78xr0gifkgjxdxlj63pw4kc8x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/m4b/scroll")
    (synopsis
     "A macros 1.1 derive implementation for Pread and Pwrite traits from the scroll crate")
    (description
     "This package provides a macros 1.1 derive implementation for Pread and Pwrite
traits from the scroll crate")
    (license license:expat)))

(define-public rust-scroll-0.11
  (package
    (name "rust-scroll")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "scroll" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1nhrhpzf95pxbcjjy222blwf8rl3adws6vsqax0yzyxsa6snbi84"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-scroll-derive" ,rust-scroll-derive-0.11))))
    (home-page "https://github.com/m4b/scroll")
    (synopsis
     "A suite of powerful, extensible, generic, endian-aware Read/Write traits for byte buffers")
    (description
     "This package provides a suite of powerful, extensible, generic, endian-aware
Read/Write traits for byte buffers")
    (license license:expat)))

(define-public rust-object-0.30
  (package
    (name "rust-object")
    (version "0.30.4")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "object" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "11f3cfd7b54ij1rwvrp9837nhszjdndxr4f4iyxazkyrhq5nid03"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-compiler-builtins" ,rust-compiler-builtins-0.1)
                       ("rust-crc32fast" ,rust-crc32fast-1)
                       ("rust-flate2" ,rust-flate2-1)
                       ("rust-hashbrown" ,rust-hashbrown-0.13)
                       ("rust-indexmap" ,rust-indexmap-1)
                       ("rust-memchr" ,rust-memchr-2)
                       ("rust-rustc-std-workspace-alloc" ,rust-rustc-std-workspace-alloc-1)
                       ("rust-rustc-std-workspace-core" ,rust-rustc-std-workspace-core-1)
                       ("rust-wasmparser" ,rust-wasmparser-0.57))))
    (home-page "https://github.com/gimli-rs/object")
    (synopsis
     "A unified interface for reading and writing object file formats.")
    (description
     "This package provides a unified interface for reading and writing object file
formats.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-vte-0.11
  (package
    (name "rust-vte")
    (version "0.11.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "vte" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "15r1ff4j8ndqj9vsyil3wqwxhhl7jsz5g58f31n0h1wlpxgjn0pm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-arrayvec" ,rust-arrayvec-0.7)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-utf8parse" ,rust-utf8parse-0.2)
                       ("rust-vte-generate-state-changes" ,rust-vte-generate-state-changes-0.1))))
    (home-page "https://github.com/alacritty/vte")
    (synopsis "Parser for implementing terminal emulators")
    (description "Parser for implementing terminal emulators")
    (license (list license:asl2.0 license:expat))))

(define-public rust-itoa-1
  (package
    (name "rust-itoa")
    (version "1.0.9")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "itoa" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0f6cpb4yqzhkrhhg6kqsw3wnmmhdnnffi6r2xzy248gzi2v0l5dg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-no-panic" ,rust-no-panic-0.1))))
    (home-page "https://github.com/dtolnay/itoa")
    (synopsis "Fast integer primitive to string conversion")
    (description "Fast integer primitive to string conversion")
    (license (list license:expat license:asl2.0))))

(define-public rust-vt100-0.15
  (package
    (name "rust-vt100")
    (version "0.15.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "vt100" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1pklc8y984axmxr0cd363srr2d27wd5rj15xlcmkjznvy0xqdkc4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-itoa" ,rust-itoa-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-unicode-width" ,rust-unicode-width-0.1)
                       ("rust-vte" ,rust-vte-0.11))))
    (home-page "https://github.com/doy/vt100-rust")
    (synopsis "Library for parsing terminal data")
    (description "Library for parsing terminal data")
    (license license:expat)))

(define-public rust-indicatif-0.17
  (package
    (name "rust-indicatif")
    (version "0.17.5")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "indicatif" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0myhjp1l6c5hd2wjm4grm6mp59ybcminxxfps5z3jfirlwiwry4g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-console" ,rust-console-0.15)
                       ("rust-instant" ,rust-instant-0.1)
                       ("rust-number-prefix" ,rust-number-prefix-0.4)
                       ("rust-portable-atomic" ,rust-portable-atomic-1)
                       ("rust-rayon" ,rust-rayon-1)
                       ("rust-tokio" ,rust-tokio-1)
                       ("rust-unicode-segmentation" ,rust-unicode-segmentation-1)
                       ("rust-unicode-width" ,rust-unicode-width-0.1)
                       ("rust-vt100" ,rust-vt100-0.15))))
    (home-page "https://github.com/console-rs/indicatif")
    (synopsis "A progress bar and cli reporting library for Rust")
    (description
     "This package provides a progress bar and cli reporting library for Rust")
    (license license:expat)))

(define-public rust-hxdmp-0.2
  (package
    (name "rust-hxdmp")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "hxdmp" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1c66j4z423w2lc3iqzzbg10y8ip58i90lpx7mimq8rklibr2fyx1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/rustyhorde/hxdmp")
    (synopsis "A small utility to create hexdump output from byte slices")
    (description
     "This package provides a small utility to create hexdump output from byte slices")
    (license (list license:expat license:asl2.0))))

(define-public rust-wchisp-0.2
  (package
    (name "rust-wchisp")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "wchisp" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1n5il94d6v4gvq0xjsdbjgdgfa769iglp395v0h3l1yjngb8b53f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anyhow" ,rust-anyhow-1)
                       ("rust-bitfield" ,rust-bitfield-0.14)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-hex" ,rust-hex-0.4)
                       ("rust-hxdmp" ,rust-hxdmp-0.2)
                       ("rust-ihex" ,rust-ihex-3)
                       ("rust-indicatif" ,rust-indicatif-0.17)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-object" ,rust-object-0.30)
                       ("rust-rand" ,rust-rand-0.8)
                       ("rust-rusb" ,rust-rusb-0.9)
                       ("rust-scroll" ,rust-scroll-0.11)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-yaml" ,rust-serde-yaml-0.9)
                       ("rust-simplelog" ,rust-simplelog-0.12))))
    (home-page "https://github.com/ch32-rs/wchisp")
    (synopsis
     "A command-line implementation of WCHISPTool, for flashing ch32 MCUs")
    (description
     "This package provides a command-line implementation of WCHISPTool, for flashing
ch32 MCUs")
    (license license:gpl2)))

(define-public chrpath
  (let ((commit "3afa271bd0fe7e6399ef32f4975f463ac07f1ca2")
        (revision "0"))
    (package
      (name "chrpath")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
	       (url "https://github.com/jwilk-mirrors/chrpath")
	       (commit commit)))
         (file-name (git-file-name name version))
         (sha256
	  (base32 "1xfnqbyc1d3v7xh1i9k8kmps3hhn87pf925bccc4c204rwm1gg8w"))))
      (build-system gnu-build-system)
      (native-inputs (list automake autoconf m4))
      (home-page "https://github.com/jwilk-mirrors/chrpath")
      (synopsis "edit rpath in ELF binaries")
      (description "like patchelf")
      (license license:gpl2))))

(define-public thead-riscv64-unknown-elf-binutils-gdb
  (let ((commit "3638a1d5884e56a7eb1eb6770ddecdbcc8e6712b")
        (revision "0")
        (xbinutils riscv64-unknown-elf-binutils))
    (package
      (inherit xbinutils)
      (name "thead-riscv64-unknown-elf-binutils-gdb")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
	       (url "https://github.com/T-head-Semi/binutils-gdb")
	       (commit commit)))
         (file-name (git-file-name name version))
         (sha256
	  (base32 "0lkd1j337hkjwqllblpvsz6yj7rixsgp16ccy4p1dlbzr86bc6hk"))))
      (native-inputs (append (list perl bison flex texinfo)
                             (package-native-inputs xbinutils)))
      (arguments
       `(#:tests? #f ,@(package-arguments xbinutils))))))

(define-public thead-riscv64-unknown-elf-gcc
  (let ((commit "64458a228d6cd007f239fcc7f9b3995df304c9b8")
        (revision "0")
        (xgcc (cross-gcc "riscv64-unknown-elf"
                         #:xbinutils thead-riscv64-unknown-elf-binutils-gdb)))
    (package
      (inherit xgcc)
      (name "thead-riscv64-unknown-elf-gcc")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
	       (url "https://github.com/T-head-Semi/gcc.git")
	       (commit commit)))
         (file-name (git-file-name name version))
         (sha256
	  (base32 "1j1318zzafbdjsssx2a6r9alqdipl4kw7mdfjhg0bzrji19zw3fa"))
         (patches
          (append
           (origin-patches (package-source riscv64-unknown-elf-gcc))))))
      (native-inputs
       (append
        `(("flex" ,flex) ("bison" ,bison))
        (package-native-inputs xgcc)))
      (arguments
       (substitute-keyword-arguments (package-arguments xgcc)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'set-paths 'augment-CPLUS_INCLUDE_PATH
                (lambda* (#:key inputs #:allow-other-keys)
                  (let ((gcc (assoc-ref inputs  "gcc")))
                    ;; Remove the default compiler from CPLUS_INCLUDE_PATH to
                    ;; prevent header conflict with the GCC from native-inputs.
                    (setenv "CPLUS_INCLUDE_PATH"
                            (string-join
                             (delete (string-append gcc "/include/c++")
                                     (string-split (getenv "CPLUS_INCLUDE_PATH")
                                                   #\:))
                             ":"))
                    (format #t
                            "environment variable `CPLUS_INCLUDE_PATH' changed to ~a~%"
                            (getenv "CPLUS_INCLUDE_PATH")))))
              (add-after 'unpack 'fix-genmultilib
                (lambda _
                  (substitute* "gcc/genmultilib"
                    (("#!/bin/sh") (string-append "#!" (which "sh"))))))))
         ((#:configure-flags flags)
          #~(append (list "--enable-multilib"
                          "--with-newlib"
                          "--with-host-libstdcxx=-static-libgcc -Wl,-Bstatic,-lstdc++,-Bdynamic -lm"
                          "--enable-plugins"
                          "--disable-decimal-float"
                          "--disable-libffi"
                          "--disable-libgomp"
                          "--disable-libmudflap"
                          "--disable-libquadmath"
                          "--disable-libssp"
                          "--disable-libstdcxx-pch"
                          "--disable-nls"
                          "--disable-shared"
                          "--disable-threads"
                          "--disable-tls")
                    (delete "--disable-multilib" #$flags)))))
      (native-search-paths
       (list (search-path-specification
              (variable "CROSS_C_INCLUDE_PATH")
              (files '("riscv64-unknown-elf/include")))
             (search-path-specification
              (variable "CROSS_CPLUS_INCLUDE_PATH")
              (files '("riscv64-unknown-elf/include"
                       "riscv64-unknown-elf/include/c++"
                       "riscv64-unknown-elf/include/c++/riscv64-unknown-elf")))
             (search-path-specification
              (variable "CROSS_LIBRARY_PATH")
              (files '("riscv64-unknown-elf/lib"))))))))

(define-public thead-riscv64-unknown-elf-toolchain
  (package
    (name "thead-riscv64-unknown-elf-toolchain")
    (version (package-version thead-riscv64-unknown-elf-gcc))
    (source #f)
    (build-system trivial-build-system)
    (propagated-inputs
     (list thead-riscv64-unknown-elf-binutils-gdb
           thead-riscv64-unknown-elf-gcc
           riscv64-unknown-elf-newlib riscv64-unknown-elf-newlib-nano))
    (arguments
     '(#:modules ((guix build union))
       #:builder
       (begin
         (use-modules (ice-9 match)
                      (guix build union))
         (match %build-inputs
           (((names . directories) ...)
            (union-build (assoc-ref %outputs "out")
                         directories)
            #t)))))
    (home-page (package-home-page gcc-toolchain))
    (synopsis (package-synopsis gcc-toolchain))
    (description (package-description gcc-toolchain))
    (license (package-license gcc-toolchain))))

(define-public toybox-static
  (package
    (inherit toybox)
    (name "toybox-static")
    (arguments
     (substitute-keyword-arguments (package-arguments toybox)
       ((#:make-flags flags)
        #~(cons "LDFLAGS=--static" #$flags))))))

toybox-static
