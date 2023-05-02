(use-modules (guix profiles))
(use-package-modules
 bash emacs emacs-xyz fontutils fonts wm gnuzilla
 version-control chromium image-viewers xdisorg android
 tls gnupg password-utils radio gawk tmux ssh hexedit
 compression linux disk pciutils less man texinfo
 python-xyz nvi admin wget curl terminals certs libusb w3m
 guile web-browsers networking gdb commencement code
 ncurses virtualization glib file graphviz video ccache
 task-management image elf embedded build-tools cmake
 gnome)

(load "../packages/embedded.scm")
(load "../packages/wm.scm")

(define-public %luhui-packages
  (list bash coreutils findutils grep sed diffutils patch gawk
        tar gzip bzip2 xz e2fsprogs btrfs-progs dosfstools pciutils
        usbutils util-linux kmod eudev less mandoc info-reader nvi
        bash-completion kbd sudo inetutils iproute wget curl iw
        wireless-tools tmux picocom openssh dropbear man-pages
        nss-certs emacs-next-pgtk emacs-company emacs-lsp-mode
        emacs-lsp-ui emacs-rime emacs-telega fontconfig emacs-geiser
        font-gnu-unifont font-terminus font-google-noto-emoji
        hikari-fix foot icecat ungoogled-chromium/wayland w3m links
        imv bemenu pinentry-tty gnupg openssl password-store
        git gnu-make rtl-sdr gqrx dump1090 psmisc htop bmon iftop
	procps gdb gcc-toolchain cscope indent ncurses fastboot
	guile-3.0-latest gkermit bvi abootimg binwalk adb blanket
	singularity (list glibc "static") linux-libre-headers
	emacs-geiser-guile virt-manager strace ltrace perf ccache
	dbus file graphviz squashfs-tools alsa-utils ffmpeg lrzsz
	openixcard grim wl-clipboard patchelf emacs-paredit
	emacs-yasnippet emacs-yasnippet-snippets emacs-magit ccls
	emacs-ccls bear cmake proxychains-ng adwaita-icon-theme
	python-extract-dtb cutter radare2))

(packages->manifest %luhui-packages)
