(use-modules
 (gnu)
 (gnu packages)
 (gnu packages compression)
 (guix packages)
 (guix build-system copy)
 (guix build-system gnu)
 (guix gexp)
 ((guix licenses) #:prefix license:)
 (guix utils)
 (guix download)
 (guix git-download))

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
  (let ((commit "c8807d4d77f2a4262e5309d08d6d2b3f8fb64d94")
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
	  (base32 "0f1inhzk42yk6xjis038mjvzy01s9adz8363q5lwydcndx11yl56"))))
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
          (delete 'strip)
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
