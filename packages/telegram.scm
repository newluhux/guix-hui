;;; Copyright (C) 2023 Lu Hui <luhux76@gmail.com>
(use-modules (guix build-system gnu)
	     (guix build-system cmake)
             (guix git-download)
             (guix download)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (guix utils)
             (guix gexp)
             (gnu packages pkg-config)
             (gnu packages image)
             (gnu packages animation))

(define-public tgs2png
  (let ((commit "25c15b7c2ca3b1a580a383d9d3cb13bf8531d04a")
	(revision "0"))
    (package
      (name "tgs2png")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
	       (url "https://github.com/zevlg/tgs2png")
	       (commit commit)))
         (file-name (git-file-name name version))
         (sha256
	  (base32 "0camvzapkfvr9v0nkk96n26rdmw0g8wbpv41i5l03j6bzdgm4myl"))))
      (build-system cmake-build-system)
      (native-inputs (list pkg-config))
      (inputs (list libpng rlottie))
      (arguments `(#:tests? #f))
      (home-page "https://github.com/zevlg/tgs2png")
      (synopsis "Convert tgs to png")
      (description "C program use for convert telegram's sticker format
to png")
      (license license:gpl3))))

