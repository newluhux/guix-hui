(use-modules (gnu)
             (gnu packages)
             (gnu packages wm)
             (gnu packages gtk)
             (gnu packages freedesktop)
             (gnu packages xdisorg)
             (gnu packages linux)
             (guix packages)
             (guix git-download))

(define-public wlroots-0.15.1
  (package
    (inherit wlroots)
    (version "0.15.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/wlroots/wlroots")
             (commit version)))
       (sha256
        (base32 "00s73nhi3sc48l426jdlqwpclg41kx1hv0yk4yxhbzw19gqpfm1h"))))))

(define-public hikari-fix
  (package
    (inherit hikari)
    (inputs
     (list cairo
           libinput-minimal
           libucl
           libxkbcommon
           linux-pam
           pango
           wayland
           wlroots-0.15.1))))

hikari-fix
