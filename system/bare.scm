(use-modules (gnu))
(use-service-modules networking)
(use-package-modules certs)

(define-public bare-os
 (operating-system
  (host-name "guix")
  (timezone "UTC")
  (locale "en_US.utf8")
  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (targets '("/dev/sdX"))))
  (file-systems (cons (file-system
                        (device (file-system-label "root"))
                        (mount-point "/")
                        (type "ext4"))
                      %base-file-systems))
  (users %base-user-accounts)
  (packages (cons* nss-certs le-certs %base-packages))
  (services (append (list (service dhcp-client-service-type))
                    %base-services))))
