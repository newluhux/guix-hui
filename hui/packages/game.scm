;;; Copyright (C) 2023 Lu Hui <luhux76@gmail.com>
(define-module (hui packages game)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages image)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages video)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages lua))

(define-public onscripter
    (package
      (name "onscripter")
      (version "20220816")
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://onscripter.osdn.jp/onscripter-"
                             version ".tar.gz"))
         (sha256
	  (base32 "1mfm8a3ndadlb7397lmigvz303h6nhmhx8z620dfjxqpll0a9gp2"))
         (patches
          (list
           (local-file
            "aux-files/onscripter/0001-remove-avifile-support.patch")
           (local-file
            "aux-files/onscripter/0002-remove-hardcoded-compiler-setup.patch")
           ))))
      (build-system gnu-build-system)
      (native-inputs (list pkg-config))
      (inputs
       `(("sdl" ,(sdl-union
                  (list sdl-ttf sdl-image sdl-mixer)))
         ("bzip2" ,bzip2)
         ("libjpeg" ,ijg-libjpeg)
         ("libsmpeg" ,libsmpeg-with-sdl1)
         ("fontconfig" ,fontconfig)
         ("libogg" ,libogg)
         ("libvorbis" ,libvorbis)
         ("lua" ,lua-5.1)))
      (arguments
       `(#:tests? #f
         #:make-flags (list (string-append "CC="
                                           ,(cxx-for-target))
                            (string-append "LD="
                                           (string-append
                                            ,(cxx-for-target)
                                            " -o"))
                            (string-append "PREFIX="
                                           (assoc-ref %outputs "out"))
                            (string-append "CFLAGS="
                                           (string-append
                                            "-L"
                                            (assoc-ref %build-inputs "lua")
                                            "/lib")))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-before 'build 'set-SDL
             (lambda* (#:key inputs #:allow-other-keys)
               (setenv "CPATH"
                       (string-append
                        (assoc-ref inputs "sdl") "/include/SDL:"
                        (or (getenv "CPATH") "")))))
           (add-before 'build 'fix-hardcoded
             (lambda* (#:key inputs #:allow-other-keys)
               (rename-file "Makefile.Linux" "Makefile")
               (substitute* "Makefile"
                 (("/usr/include/lua5.1")
                  (string-append (assoc-ref inputs "lua")
                                 "/include/lua5.1"))))))))
      (home-page "https://onscripter.osdn.jp/onscripter.html")
      (synopsis "interprets and executes scripts written for NScripter
in its own way.")
      (description "")
      (license license:gpl2)))


onscripter


