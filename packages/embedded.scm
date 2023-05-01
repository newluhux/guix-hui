;;; Copyright (C) 2023 Lu Hui <luhux76@gmail.com>

(use-modules (guix build-system gnu)
	     (guix build-system cmake)
             (guix build-system python)
             (guix build-system trivial)
             (guix git-download)
             (guix download)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (guix utils)
             (guix gexp)
             (gnu packages cpp)
             (gnu packages textutils)
             (gnu packages autotools)
             (gnu packages m4)
             (gnu packages pkg-config)
             (gnu packages base)
             (gnu packages genimage)
             (gnu packages python-crypto)
             (gnu packages check)
             (gnu packages databases)
             (gnu packages sphinx)
             (gnu packages python-xyz)
             (gnu packages cross-base)
             (gnu packages embedded)
             (gnu packages texinfo)
             (gnu packages gdb)
             (gnu packages commencement))

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


(define-public python-pycryptodome-3.9.8
  (package
    (inherit python-pycryptodome)
    (version "3.9.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pycryptodome" version))
       (sha256
        (base32
         "195xi12i1vwm3ra64gyccafz0j506d7rsskxq5fvq88hy0f1f90f"))))))


(define-public python-bflb-crypto-plus
  (package
    (name "python-bflb-crypto-plus")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "bflb_crypto_plus" version))
              (sha256
               (base32
                "0hk2cv7aav3zczxfjgbvcbwpcjyrb9qmjlpqz54wnbjx9s3q7d5i"))))
    (build-system python-build-system)
    (propagated-inputs (list python-pycryptodome))
    (home-page "https://pypi.org/project/bflb-crypto-plus")
    (synopsis "PyCrypto Cipher extension(bouffalolab version)")
    (description "PyCrypto Cipher extension(bouffalolab version)")
    (license #f))) ; TODO: add license


(define-public python-portalocker
  (package
    (name "python-portalocker")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "portalocker" version))
              (sha256
               (base32
                "1r165ll4gnsbgvf9cgdkbg04526aqwllwa7hxlkl34dah7npwj0l"))))
    (build-system python-build-system)
    (native-inputs (list python-pytest
                         python-pytest-cov
                         python-pytest-mypy
                         python-pytest-timeout
                         python-redis
                         python-sphinx))
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/WoLpH/portalocker")
    (synopsis "Wraps the portalocker recipe for easy usage")
    (description "Wraps the portalocker recipe for easy usage")
    (license #f))) ; TODO: add license

;; sucks project, bundle a lot of binary, a pice of shit.
(define-public python-pycklink
  (package
    (name "python-pycklink")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pycklink" version))
              (sha256
               (base32
                "0jzym95kb6ql8mjc82yw99lrygkarjw93lcfr8g5kr3mcppxmgai"))))
    (build-system python-build-system)
    ;; TODO: patch binary, :(
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'validate-runpath)
         (delete 'strip))))
    (home-page "https://pypi.org/project/PyCKLink/")
    (synopsis "Python interface for the T-HEAD CKLink")
    (description "Python interface for the T-HEAD CKLink")
    (license license:expat)))

(define-public python-pylink-square
  (package
    (name "python-pylink-square")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pylink-square" version))
              (sha256
               (base32
                "0dyz6c4c6bmkgxi1xys495xr889jm55ibbkrawsbz0x2kgq3j12c"))))
    (build-system python-build-system)
    (propagated-inputs (list python-future python-psutil python-six))
    (arguments
     `(#:tests? #f))
    (home-page "http://www.github.com/Square/pylink")
    (synopsis "Python interface for SEGGER J-Link.")
    (description "Python interface for SEGGER J-Link.")
    (license license:asl2.0)))

;; build is ok, but can't run.
(define-public python-bflb-mcu-tool
  (package
    (name "python-bflb-mcu-tool")
    (version "1.8.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "bflb-mcu-tool" version))
              (sha256
               (base32
                "0lj9gqdinissw670yf03xaba6ryaz5vqhdm839dhjyasc5vn5f0a"))))
    (build-system python-build-system)
    (propagated-inputs (list python-bflb-crypto-plus
                             python-ecdsa
                             python-portalocker
                             python-pycklink ; need fix
                             python-pycryptodome-3.9.8
                             python-pylink-square
                             python-pyserial))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'validate-runpath)
         (delete 'strip))))
    (home-page "https://pypi.org/project/bflb-mcu-tool/")
    (synopsis "Bouffalolab Mcu Tool")
    (description "Bouffalolab Mcu Tool")
    (license #f)))

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
