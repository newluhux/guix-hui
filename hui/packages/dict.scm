(define-module (hui packages dict)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 regex))


(define-public stardict-ecdict
  (package
    (name "stardict-ecdict")
    (version "2017")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/"
                                  "skywind3000/ECDICT/releases/download/"
                                  "1.0.28/ecdict-stardict-28.zip"))
              (sha256
               (base32
                "0b3gijq9qzyrp87i575gllyp0x70fhalmnk6cjwpkv6nvvrx01y7"))))
    (build-system copy-build-system)
    (native-inputs (list unzip))
    (arguments
     `(#:install-plan '(("stardict-ecdict-2.4.2.idx"
                         "share/stardict/stardict-ecdict-2.4.2.idx")
                        ("stardict-ecdict-2.4.2.ifo"
                         "share/stardict/stardict-ecdict-2.4.2.ifo")
                        ("stardict-ecdict-2.4.2.dict"
                         "share/stardict/stardict-ecdict-2.4.2.dict"))))
    (home-page "https://github.com/skywind3000/ECDICT")
    (synopsis "Free English to Chinese Dictionary Database")
    (description "Free English to Chinese Dictionary Database")
    (license license:expat)))

(define-public ustardict
  (let ((commit "34ce566169397ce1485f7da40abf8969239cc631")
	(revision "0"))
    (package
      (name "ustardict")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
	       (url "https://github.com/newluhux/ustardict")
	       (commit commit)))
         (file-name (git-file-name name version))
         (sha256
	  (base32 "1nldmkf2h3x57dfn0n3r5kwi196anw09xr09fi8y8829a0w3529r"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f
	 #:make-flags (if ,(string-match "mingw" (if (%current-target-system) (%current-target-system) (%current-system)))
	       (list "-Ofast" "-static" "-lwsock32")
	       (list "-Ofast" "-static"))
	 #:phases
	 (modify-phases
	  %standard-phases
	  (delete 'configure)
          (delete 'strip)
	  (replace
           'build
	   (lambda* (#:key make-flags #:allow-other-keys)
		    (apply invoke ,(cc-for-target) "ustardict.c" "-o"
			   "ustardict" make-flags)))
	  (replace
	      'install
	    (lambda* (#:key outputs #:allow-other-keys)
	      (let* ((out (assoc-ref outputs "out"))
		     (bin (string-append out "/bin")))
		(mkdir-p bin)
                (for-each
                 (lambda (file)
                   (copy-file
                    file (string-append bin "/" file)))
                 (find-files
                  "./"
                  "^(ustardict|ustardict.exe)$"))))))))
      (home-page "https://github.com/newluhux/ustardict")
      (synopsis "Simple stardict program")
      (description "Simple stardict program, writen by c")
      (license license:expat))))

ustardict
