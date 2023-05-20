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
 gnome engineering rust-apps freedesktop gnome-xyz qt
 screen xorg)

(define load-list
  (list
   "../packages/dict.scm"
   "../packages/embedded.scm"
   "../packages/engineering.scm"
   "../packages/proxy.scm"))

(map load load-list)

(define-public %luhui-packages
  (list bash coreutils findutils grep sed diffutils patch gawk
        tar gzip bzip2 xz util-linux less mandoc info-reader nvi
        bash-completion wget curl iw glibc-locales wireless-tools
	tmux picocom openssh dropbear man-pages nss-certs
	emacs-next emacs-company emacs-lsp-mode emacs-lsp-ui
	emacs-rime emacs-telega fontconfig emacs-geiser font-gnu-unifont
	font-terminus font-google-noto-emoji icecat
	ungoogled-chromium w3m links pinentry-tty gnupg
	openssl password-store git gnu-make rtl-sdr gqrx dump1090
	psmisc htop bmon iftop procps gdb gcc-toolchain cscope indent
	ncurses fastboot guile-3.0-latest gkermit bvi abootimg binwalk
	adb blanket (list glibc "static") linux-libre-headers
	emacs-geiser-guile virt-manager strace ltrace perf ccache
	dbus file graphviz squashfs-tools alsa-utils ffmpeg lrzsz
	openixcard grim wl-clipboard patchelf emacs-paredit
	emacs-yasnippet emacs-yasnippet-snippets emacs-magit ccls
	emacs-ccls cmake proxychains-ng python-extract-dtb
	blisp openocd xfel ripgrep emacs-ripgrep fzf emacs-fzf
	neofetch ek eksw screen bear iperf unzip unrar-free
	stardict-ecdict rizin emacs-ivy cutter-rizin clash
	cwm xterm xinitrc-xsession xset xsetroot xkbset xrdb))

(packages->manifest %luhui-packages)
