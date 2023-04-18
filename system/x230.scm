;;; Copyright (C) Lu Hui <luhux76@gmail.com>

(use-modules (gnu))
(use-service-modules base dbus desktop networking sound sysctl xorg docker)
(use-package-modules bash emacs emacs-xyz fontutils fonts wm gnuzilla
                     version-control chromium image-viewers xdisorg
                     tls gnupg password-utils radio gawk tmux ssh
                     compression linux disk pciutils less man texinfo
                     nvi admin wget curl terminals certs libusb w3m guile
		     web-browsers networking gdb commencement code ncurses)

(load "../packages/embedded.scm")

(define-public x230-os
  (operating-system
    (host-name "x230")
    (timezone "Hongkong")
    (locale "en_US.utf8")
    (keyboard-layout (keyboard-layout "us" #:options '("ctrl:nocaps")))

    (bootloader (bootloader-configuration
                  (bootloader grub-bootloader)
                  (targets '("/dev/sda"))))

    (file-systems (cons* (file-system
                           (device (file-system-label "x230-root"))
                           (mount-point "/")
                           (type "btrfs")) %base-file-systems))
    
    (users (cons* (user-account
                    (name "luhui")
                    (comment "Lu Hui")
                    (group "users")

                    (supplementary-groups '("wheel" "dialout" "audio" "video" "docker")))
                  %base-user-accounts))

    (skeletons `(("." ,(local-file "../home" "skel" #:recursive? #t))))

    (packages (list bash coreutils findutils grep sed diffutils patch gawk
                    tar gzip bzip2 xz e2fsprogs btrfs-progs dosfstools pciutils
                    usbutils util-linux kmod eudev less mandoc info-reader nvi
                    bash-completion kbd sudo inetutils iproute wget curl iw
                    wireless-tools tmux picocom openssh dropbear man-pages
                    nss-certs emacs-next-pgtk emacs-company emacs-lsp-mode
                    emacs-lsp-ui emacs-rime emacs-telega fontconfig emacs-geiser
                    font-gnu-unifont font-terminus font-google-noto-emoji
                    hikari foot icecat ungoogled-chromium/wayland w3m links
                    imv bemenu pinentry-tty gnupg openssl password-store
                    git gnu-make rtl-sdr gqrx dump1090 psmisc htop bmon iftop
		    procps gdb gcc-toolchain cscope indent ncurses
	            guile-3.0-latest gkermit))

    (services
     (list (service login-service-type)
           (service syslog-service-type)
           (service virtual-terminal-service-type)
           (service agetty-service-type
                    (agetty-configuration (extra-options '("-L")) ;no carrier detect
                                          (term "vt100")
                                          (tty #f) ;automatic
                                          (shepherd-requirement '(syslogd))))
           (service mingetty-service-type
                    (mingetty-configuration (tty "tty1")))
           (service mingetty-service-type
                    (mingetty-configuration (tty "tty2")))
           (service mingetty-service-type
                    (mingetty-configuration (tty "tty3")))
           (service mingetty-service-type
                    (mingetty-configuration (tty "tty4")))
           (service static-networking-service-type
                    (list %loopback-static-networking))
           (service urandom-seed-service-type)
           (service guix-service-type)
           (service nscd-service-type)
           (service udev-service-type
                    (udev-configuration (rules (list lvm2 fuse alsa-utils crda))))
           (service sysctl-service-type)
           (service special-files-service-type
                    `(("/bin/sh" ,(file-append bash "/bin/sh"))
                      ("/usr/bin/env" ,(file-append coreutils "/bin/env"))))
           (simple-service 'mtp udev-service-type
                           (list libmtp)) ;my phone
           (service network-manager-service-type)
           (service wpa-supplicant-service-type) ;needed by NetworkManager
           (service udisks-service-type)
           (service elogind-service-type)
           (service dbus-root-service-type)
           (service ntp-service-type)
           (service pulseaudio-service-type)
           (service alsa-service-type)
	   (service gpm-service-type)
	   (service docker-service-type)
	   (service singularity-service-type)
	   (service pam-limits-service-type
                    (list
		     (pam-limits-entry "luhui" 'both 'core 1048576)
		     (pam-limits-entry "luhui" 'both 'nproc 1048576)))
           (service screen-locker-service-type
                    (screen-locker-configuration "hikari-unlocker"
                                                 (file-append hikari
                                                  "/bin/hikari-unlocker") #f))
           (service screen-locker-service-type
                    (screen-locker-configuration "vlock"
                                                 (file-append kbd "/bin/vlock")
                                                 #f))))))

x230-os
