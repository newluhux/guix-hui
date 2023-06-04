(use-modules (gnu) (guix profiles))
(use-package-modules compression)

(define-public %archive-packages
  (list unzip unrar-free p7zip squashfs-tools))

(packages->manifest %archive-packages)
