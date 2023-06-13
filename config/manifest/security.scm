(use-modules (gnu) (guix profiles))
(use-package-modules password-utils gnupg tls certs)

(define-public %security-packages
  (list
   nss-certs le-certs
   password-store gnupg pinentry-tty openssl))

(packages->manifest %security-packages)
