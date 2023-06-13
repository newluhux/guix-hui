;;; Copyright (C) 2023 Lu Hui <luhux76@gmail.com>

(define-module (hui packages embedded)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
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
  #:use-module (gnu packages ncurses))

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

(define-public libsigrok
  (let ((commit "0db1b189bee3ffe5c6ea39d7ca2e62715856b538")
	(revision "0"))
    (package
     (name "libsigrok")
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

(define-public libsigrokdecode
  (let ((commit "73cb5461acdbd007f4aa9e81385940fad6607696")
	(revision "0"))
    (package
     (name "libsigrokdecode")
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

(define-public pluseview
  (let ((commit "136995b831c50d3261143b1183c73af55c9ba3a5")
	(revision "0"))
    (package
     (name "pluseview")
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
     (inputs (list boost glib libsigrok libserialport libftdi hidapi qtbase-5
		   bluez nettle glibmm libzip libsigrokdecode python qtsvg-5
		   qttools-5))
     (arguments `(#:tests? #f))
     (home-page "http://sigrok.org/wiki/PulseView")
     (synopsis "Qt based logic analyzer, oscilloscope and MSO GUI for sigrok.")
     (description "It can acquire samples from a supported device and display
them, load and display captures from existing sigrok *.sr files, as well as run
protocol decoders and display their annotations. ")
     (license license:gpl3))))

(define-public sigrok-cli
  (let ((commit "394fd9b7a456f16c7ac15f41b0e29081f1d951f8")
	(revision "0"))
    (package
     (name "sigrok-cli")
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

(define-public ufbterm
  (let ((commit "ddcce9d79c0c3578b95a0d1372ee86e8ec31944b")
	(revision "0"))
    (package
      (name "ufbterm")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
	       (url "https://github.com/newluhux/ufbterm")
	       (commit commit)))
         (file-name (git-file-name name version))
         (sha256
	  (base32 "1sywafsvp40ksq0am8q50dfsa4j1k0grizy6c4sfil9bc97gwypq"))))
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
		      (copy-file "ufbterm" (string-append bin "/ufbterm"))))))))
      (home-page "https://github.com/newluhux/ufbterm")
      (synopsis "Simple terminal on framebuffer")
      (description "Simple terminal on framebuffer (WIP)")
      (license license:expat))))

ufbterm
