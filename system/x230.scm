(use-modules (gnu) (gnu system nss) (guix utils))
(use-service-modules desktop xorg virtualization docker sddm)
(use-package-modules certs linux gnome wm radio admin embedded)

(load "../packages/embedded.scm")
(load "../packages/linux.scm")

(operating-system
  (kernel linux-stable)
  (kernel-loadable-modules (list rtl8812au-aircrack-ng-linux-module))
  (firmware (list linux-firmware))
  (kernel-arguments (list "modprobe.blacklist=dvb_usb_rtl28xxu"))

  (host-name "x230")
  (timezone "Hongkong")
  (locale "en_US.utf8")

  (keyboard-layout (keyboard-layout "us" #:options '("ctrl:nocaps")))

  (bootloader (bootloader-configuration
               (bootloader grub-bootloader)
               (targets '("/dev/sda"))
               (keyboard-layout keyboard-layout)))

  (mapped-devices
   (list (mapped-device
          (source (uuid "dd506c9b-ae0b-40dc-a3bd-9677a7410e53"))
          (target "x230-root")
          (type luks-device-mapping))))

  (file-systems (append
                 (list (file-system
                         (device (uuid "0765e38f-ca19-4d8e-af99-3299f8ccfdf0"))
                         (mount-point "/")
                         (type "btrfs")
                         (dependencies mapped-devices))
                       (file-system
                         (device (uuid "b45765a5-9995-4147-8eae-d123775ec99b"))
                         (mount-point "/boot")
                         (type "ext4")))
                 %base-file-systems))

  (swap-devices (list (swap-space
                       (target "/swapfile"))))

  (users (cons (user-account
                (name "luhui")
                (comment "Lu Hui")
                (group "users")
                (supplementary-groups '("wheel" "netdev" "cdrom" "docker"
                                        "audio" "video" "dialout" "kvm")))
               %base-user-accounts))

  (packages
   (append
    (list
     singularity
     gnome-tweaks
     btrfs-progs
     xinitrc-xsession)
    %base-packages))

  (services
   (cons*
    (service docker-service-type)
    (service singularity-service-type)
    (service qemu-binfmt-service-type
             (qemu-binfmt-configuration
              (platforms
               (lookup-qemu-platforms
                "arm" "aarch64" "riscv64"))))
    (udev-rules-service 'libsigrok libsigrok)
    (udev-rules-service 'rtl-sdr rtl-sdr)
    (udev-rules-service 'xfel xfel)
    (udev-rules-service 'openocd openocd)
    (service sddm-service-type)
    (set-xorg-configuration
     (xorg-configuration
      (keyboard-layout keyboard-layout))
     sddm-service-type)
    (modify-services %desktop-services
      (delete gdm-service-type))))

  (name-service-switch %mdns-host-lookup-nss))
