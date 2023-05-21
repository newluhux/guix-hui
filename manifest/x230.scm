(use-modules (gnu) (guix profiles))


(define load-list
  (list
   "archive.scm"
   "connect.scm"
   "develop.scm"
   "dict.scm"
   "emacs.scm"
   "media.scm"
   "font.scm"
   "monitor.scm"
   "radio.scm"
   "security.scm"
   "proxy.scm"
   "xorg.scm"
   "web.scm"))

(map load load-list)

(define-public %x230-packages
  (append
   %archive-packages
   %connect-packages
   %develop-packages
   %dict-packages
   %emacs-packages
   %font-packages
   %media-packages
   %monitor-packages
   %radio-packages
   %security-packages
   %xorg-packages
   %proxy-packages
   %web-packages))

(packages->manifest %x230-packages)
