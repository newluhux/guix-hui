;;; Copyright (C) Lu Hui <luhux76@gmail.com>

(use-modules (gnu))
(use-service-modules base dbus desktop networking sound sysctl xorg docker
		     virtualization pm)
(use-package-modules bash emacs emacs-xyz fontutils fonts wm gnuzilla
                     version-control chromium image-viewers xdisorg android
                     tls gnupg password-utils radio gawk tmux ssh hexedit
                     compression linux disk pciutils less man texinfo python-xyz
                     nvi admin wget curl terminals certs libusb w3m guile
		     web-browsers networking gdb commencement code ncurses
		     virtualization glib file graphviz video ccache
	             task-management image elf embedded build-tools)

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

    (kernel-arguments '("modprobe.blacklist=dvb_usb_rtl28xxu"))
    (file-systems (cons* (file-system
                           (device (file-system-label "x230-root"))
                           (mount-point "/")
                           (type "btrfs"))
			 (file-system
                           (device "/dev/mmcblk0p1")
                           (mount-point "/sdcard")
                           (type "vfat"))
			 %base-file-systems))

    (users (cons* (user-account
                    (name "luhui")
                    (comment "Lu Hui")
                    (group "users")

                    (supplementary-groups '("wheel" "dialout" "audio" "video"
					    "docker" "libvirt")))
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
		    procps gdb gcc-toolchain cscope indent ncurses fastboot
	            guile-3.0-latest gkermit bvi abootimg binwalk adb blanket
	            singularity (list glibc "static") linux-libre-headers
		    emacs-geiser-guile virt-manager strace ltrace perf ccache
	            dbus file graphviz squashfs-tools alsa-utils ffmpeg lrzsz
	            openixcard grim wl-clipboard patchelf gdb-arm-none-eabi
	            arm-none-eabi-nano-toolchain-6 emacs-paredit emacs-yasnippet
	            emacs-yasnippet-snippets emacs-magit ccls emacs-ccls bear))

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
                    (udev-configuration (rules (list lvm2 fuse alsa-utils crda
                                                 rtl-sdr))))
           (service sysctl-service-type)
           (service special-files-service-type
                    `(("/bin/sh" ,(file-append bash "/bin/sh"))
                      ("/usr/bin/env" ,(file-append coreutils "/bin/env"))))
           (simple-service 'mtp udev-service-type
                           (list libmtp)) ;my phone
           (service network-manager-service-type)
           (service wpa-supplicant-service-type)
           (service udisks-service-type)
           (service elogind-service-type)
	   (service polkit-service-type)
           (service dbus-root-service-type)
           (service ntp-service-type)
           (service pulseaudio-service-type)
           (service alsa-service-type)
	   (service gpm-service-type)
	   (service docker-service-type)
	   (service singularity-service-type)
	   (service tor-service-type
                    (tor-configuration
                     (config-file (plain-file "tor-config"
                                              "HTTPTunnelPort 127.0.0.1:9250\n
Socks5Proxy 127.0.0.1:7891"))))
	   (service pam-limits-service-type
                    (list
		     (pam-limits-entry "luhui" 'both 'core 1048576)
		     (pam-limits-entry "luhui" 'both 'nproc 1048576)))
	   (service libvirt-service-type
		    (libvirt-configuration
		     (unix-sock-group "libvirt")))
	   (service virtlog-service-type)
	   (service qemu-binfmt-service-type
                    (qemu-binfmt-configuration
                     (platforms (lookup-qemu-platforms "arm" "aarch64"
						       "riscv32" "riscv64"))))
	   (service tlp-service-type
                    (tlp-configuration
                     (cpu-scaling-governor-on-ac (list "ondenmand"))
                     (sched-powersave-on-bat? #t)))
           (service screen-locker-service-type
                    (screen-locker-configuration "hikari-unlocker"
                                                 (file-append hikari
                                                  "/bin/hikari-unlocker") #f))
           (service screen-locker-service-type
                    (screen-locker-configuration "vlock"
                                                 (file-append kbd "/bin/vlock")
                                                 #f))))))

x230-os
