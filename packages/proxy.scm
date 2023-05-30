(use-modules
 (gnu)
 (gnu packages)
 (gnu packages golang)
 (gnu packages syncthing)
 (gnu packages databases)
 (guix packages)
 (guix build-system copy)
 (guix build-system go)
 (guix gexp)
 ((guix licenses) #:prefix license:)
 (guix utils)
 (guix download)
 (guix git-download))

(define-public go-github-com-dreamacro-protobytes
  (package
    (name "go-github-com-dreamacro-protobytes")
    (version "0.0.0-20230524072133-0b6ef2348cfa")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Dreamacro/protobytes")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1xlqi55hgj7nnv9by18d6kijvv0aprisgr7spf2gkh0wk5hcdhkl"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/Dreamacro/protobytes"
       #:go ,go-1.20
       #:tests? #f))
    (propagated-inputs `(("go-gopkg-in-yaml-v3" ,go-gopkg-in-yaml-v3)
                         ("go-github-com-pmezard-go-difflib" ,go-github-com-pmezard-go-difflib)
                         ("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)
                         ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
    (home-page "https://github.com/Dreamacro/protobytes")
    (synopsis #f)
    (description #f)
    (license #f)))

(define-public go-github-com-go-chi-cors
  (package
    (name "go-github-com-go-chi-cors")
    (version "1.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/go-chi/cors")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13ya3h4lsd18hs58ark5q31dr2dpszgbjr371rph3yphfp2ddzlg"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/go-chi/cors"))
    (home-page "https://github.com/go-chi/cors")
    (synopsis "CORS net/http middleware")
    (description
     "cors package is net/http handler to handle CORS related requests as defined by
@@url{http://www.w3.org/TR/cors/,http://www.w3.org/TR/cors/}")
    (license license:expat)))

(define-public go-github-com-go-chi-render
  (package
    (name "go-github-com-go-chi-render")
    (version "1.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/go-chi/render")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0lp6gczqc86515079pmhmw0h4kxp6wb5rj2fhf1xbs08369535al"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/go-chi/render"))
    (propagated-inputs `(("go-github-com-ajg-form" ,go-github-com-ajg-form)))
    (home-page "https://github.com/go-chi/render")
    (synopsis "render")
    (description
     "The @@code{render} package helps manage HTTP request / response payloads.")
    (license license:expat)))

(define-public go-github-com-gofrs-uuid-v5
  (package
    (name "go-github-com-gofrs-uuid-v5")
    (version "5.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/gofrs/uuid")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0lx08k02vwm1zbzb7b9yg69rqhwjla5frlw29vvlb6c6yn75fjch"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gofrs/uuid/v5"))
    (home-page "https://github.com/gofrs/uuid")
    (synopsis "UUID")
    (description
     "Package uuid provides implementations of the Universally Unique Identifier
(UUID), as specified in RFC-4122 and the Peabody RFC Draft (revision 03).")
    (license license:expat)))

(define-public go-github-com-fanliao-go-promise
  (package
    (name "go-github-com-fanliao-go-promise")
    (version "0.0.0-20141029170127-1890db352a72")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/fanliao/go-promise")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0nlls86fx6sxsvwp5k769h5knwh96j8fahhivh6fagzjjyyqcijd"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/fanliao/go-promise"
       #:tests? #f))
    (home-page "https://github.com/fanliao/go-promise")
    (synopsis "Installation")
    (description
     "Package promise provides a complete promise and future implementation.  A quick
start sample:")
    (license license:expat)))

(define-public go-github-com-hugelgupf-socketpair
  (package
    (name "go-github-com-hugelgupf-socketpair")
    (version "0.0.0-20190730060125-05d35a94e714")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hugelgupf/socketpair")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1kc243zdrh8n8wmcylyzam3gcqd6zbqnn9a5liaa4jhvn8rwym3j"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/hugelgupf/socketpair"))
    (home-page "https://github.com/hugelgupf/socketpair")
    (synopsis "socketpair")
    (description
     "Package socketpair provides bidirectionally connected net.Conns.")
    (license license:bsd-3)))

(define-public go-github-com-cilium-ebpf
  (package
    (name "go-github-com-cilium-ebpf")
    (version "0.10.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cilium/ebpf")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1syyyp4y2hq31lbvz1hnl2zvww02vmnc2b8dm9k4m7nmxf64sxyy"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/cilium/ebpf"
       #:go ,go-1.20
       #:tests? #f))
    (propagated-inputs `(("go-github-com-rogpeppe-go-internal" ,go-github-com-rogpeppe-go-internal)
                         ("go-github-com-kr-text" ,go-github-com-kr-text)
                         ("go-github-com-kr-pretty" ,go-github-com-kr-pretty)
                         ("go-golang-org-x-sys" ,go-golang-org-x-sys)
                         ("go-github-com-google-go-cmp" ,go-github-com-google-go-cmp)
                         ("go-github-com-frankban-quicktest" ,go-github-com-frankban-quicktest)))
    (home-page "https://github.com/cilium/ebpf")
    (synopsis "eBPF")
    (description "Package ebpf is a toolkit for working with eBPF programs.")
    (license license:expat)))

(define-public go-github-com-jsimonetti-rtnetlink
  (package
    (name "go-github-com-jsimonetti-rtnetlink")
    (version "1.3.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jsimonetti/rtnetlink")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "163nkfkp52cyy6i31g69kbhzghn29n479gklim0cm0hkms20rfsq"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/jsimonetti/rtnetlink"
       #:go ,go-1.20))
    (propagated-inputs `(("go-golang-org-x-sync" ,go-golang-org-x-sync)
                         ("go-golang-org-x-net" ,go-golang-org-x-net)
                         ("go-github-com-mdlayher-socket" ,go-github-com-mdlayher-socket)
                         ("go-github-com-josharian-native" ,go-github-com-josharian-native)
                         ("go-golang-org-x-sys" ,go-golang-org-x-sys)
                         ("go-github-com-mdlayher-netlink" ,go-github-com-mdlayher-netlink)
                         ("go-github-com-google-go-cmp" ,go-github-com-google-go-cmp)
                         ("go-github-com-cilium-ebpf" ,go-github-com-cilium-ebpf)))
    (home-page "https://github.com/jsimonetti/rtnetlink")
    (synopsis "rtnetlink")
    (description
     "Package rtnetlink allows the kernel's routing tables to be read and altered.
Network routes, IP addresses, Link parameters, Neighbor setups, Queueing
disciplines, Traffic classes and Packet classifiers may all be controlled.  It
is based on netlink messages.")
    (license license:expat)))

(define-public go-github-com-mdlayher-packet
  (package
    (name "go-github-com-mdlayher-packet")
    (version "1.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mdlayher/packet")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17jms60j1xygsn5z61r2kz8yywsihf0xjzlhq3v5cpqf08mljwsn"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/mdlayher/packet"
       #:go ,go-1.20
       #:tests? #f))
    (propagated-inputs `(("go-golang-org-x-sync" ,go-golang-org-x-sync)
                         ("go-golang-org-x-sys" ,go-golang-org-x-sys)
                         ("go-golang-org-x-net" ,go-golang-org-x-net)
                         ("go-github-com-mdlayher-socket" ,go-github-com-mdlayher-socket)
                         ("go-github-com-josharian-native" ,go-github-com-josharian-native)
                         ("go-github-com-google-go-cmp" ,go-github-com-google-go-cmp)))
    (home-page "https://github.com/mdlayher/packet")
    (synopsis "packet")
    (description
     "Package packet provides access to Linux packet sockets (AF_PACKET).")
    (license license:expat)))

(define-public go-github-com-jtolds-gls
  (package
    (name "go-github-com-jtolds-gls")
    (version "v4.20.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jtolio/gls")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1k7xd2q2ysv2xsh373qs801v6f359240kx0vrl0ydh7731lngvk6"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/jtolds/gls"))
    (home-page "https://github.com/jtolds/gls")
    (synopsis "gls")
    (description "Package gls implements goroutine-local storage.")
    (license license:expat)))

(define-public go-github-com-smartystreets-assertions
  (package
    (name "go-github-com-smartystreets-assertions")
    (version "1.13.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/smartystreets/assertions")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02jkx1xi6y7zfqhzk1pkrpxn5fcv5fh7lw6q7pn9vfj8zsjfd7l3"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/smartystreets/assertions"
       #:go ,go-1.20))
    (home-page "https://github.com/smartystreets/assertions")
    (synopsis
     "SMARTY DISCLAIMER: Subject to the terms of the associated license agreement, this software is freely available for your use. This software is FREE, AS IN PUPPIES, and is a gift. Enjoy your new responsibility. This means that while we may consider enhancement requests, we may or may not choose to entertain requests at our sole and absolute discretion.")
    (description
     "Package assertions contains the implementations for all assertions which are
referenced in goconvey's `convey` package
(github.com/smartystreets/goconvey/convey) and gunit
(github.com/smartystreets/gunit) for use with the So(...) method.  They can also
be used in traditional Go test functions and even in applications.")
    (license license:expat)))

(define-public go-github-com-neelance-astrewrite
  (package
    (name "go-github-com-neelance-astrewrite")
    (version "0.0.0-20160511093645-99348263ae86")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/neelance/astrewrite")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "07527807p8q6h05iq4qy0xrlcmwyzj76gpk0yqf71yaj447mz24v"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/neelance/astrewrite"))
    (home-page "https://github.com/neelance/astrewrite")
    (synopsis #f)
    (description #f)
    (license license:bsd-2)))

(define-public go-github-com-neelance-sourcemap
  (package
    (name "go-github-com-neelance-sourcemap")
    (version "0.0.0-20200213170602-2833bce08e4c")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/neelance/sourcemap")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05ymjg1z9phf0wp4w058kvf13bmn4skv67chb3r04z69in8y8jih"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/neelance/sourcemap"))
    (home-page "https://github.com/neelance/sourcemap")
    (synopsis #f)
    (description #f)
    (license license:bsd-2)))

(define-public go-github-com-shurcool-go-browser
  (package
    (name "go-github-com-shurcool-go-vfs")
    (version "0.0.0-20200502201357-93f07166e636")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/shurcooL/go")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wgwlhsgx1c2v650xvf099hgrd4av18gfb0kha09klmsh0p0hc5r"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/shurcooL/go/browser"
       #:unpack-path "github.com/shurcooL/go"))
    (home-page "https://github.com/shurcooL/go")
    (synopsis "go")
    (description "Common Go code.")
    (license license:expat)))

(define-public go-github-com-shurcool-go-gddo
  (package
    (inherit go-github-com-shurcool-go-browser)
    (name "go-github-com-shurcool-go-vfs")
    (arguments
     `(#:import-path "github.com/shurcooL/go/gddo"
       #:unpack-path "github.com/shurcooL/go"))))

(define-public go-github-com-shurcool-httpfs-filter
  (package
    (name "go-github-com-shurcool-httpfs-filter")
    (version "0.0.0-20190707220628-8d4bc4ba7749")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/shurcooL/httpfs")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qjkbjnp86kjr7r0xjwp39blnk1ggkzy6zm3xphr5dpin4jkgfa1"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/shurcooL/httpfs/filter"
       #:unpack-path "github.com/shurcooL/httpfs"
       #:tests? #f))
    (home-page "https://github.com/shurcooL/httpfs")
    (synopsis "httpfs")
    (description
     "Collection of Go packages for working with the
@@url{https://godoc.org/net/http#FileSystem,(code http.FileSystem)} interface.")
    (license license:expat)))

(define-public go-github-com-shurcool-httpfs-vfsutil
  (package
    (inherit go-github-com-shurcool-httpfs-filter)
    (name "go-github-com-shurcool-httpfs-vfsutil")
    (arguments
     '(#:import-path "github.com/shurcooL/httpfs/vfsutil"
       #:unpack-path "github.com/shurcooL/httpfs"
       #:tests? #f))))

(define-public go-github-com-inconshreveable-mousetrap
  (package
    (name "go-github-com-inconshreveable-mousetrap")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/inconshreveable/mousetrap")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14gjpvwgx3hmbd92jlwafgibiak2jqp25rq4q50cq89w8wgmhsax"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/inconshreveable/mousetrap"))
    (home-page "https://github.com/inconshreveable/mousetrap")
    (synopsis "mousetrap")
    (description "mousetrap is a tiny library that answers a single question.")
    (license license:asl2.0)))

(define-public go-github-com-shurcool-vfsgen
  (package
    (name "go-github-com-shurcool-vfsgen")
    (version "0.0.0-20200824052919-0d455de96546")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/shurcooL/vfsgen")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0md1vgaq95x1jmxpnsfv6s9xf3v8gqi7lcl7mkxpf6274rf1n2pk"))))
    (build-system go-build-system)
    (propagated-inputs `(("go-github-com-shurcool-httpfs-vfsutil"
                          ,go-github-com-shurcool-httpfs-vfsutil)))
    (arguments
     '(#:import-path "github.com/shurcooL/vfsgen"
       #:tests? #f))
    (home-page "https://github.com/shurcooL/vfsgen")
    (synopsis "vfsgen")
    (description
     "Package vfsgen takes an http.FileSystem (likely at `go generate` time) and
generates Go code that statically implements the provided http.FileSystem.")
    (license license:expat)))

(define-public go-github-com-gopherjs-gopherjs
  (package
    (name "go-github-com-gopherjs-gopherjs")
    (version "1.17.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/gopherjs/gopherjs")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1dg22b3pw0g3g0a84fmzl0fl76540yjvlcvscw2876gdy5db98n3"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gopherjs/gopherjs"))
    (propagated-inputs `(("go-golang-org-x-xerrors" ,go-golang-org-x-xerrors)
                         ("go-golang-org-x-term" ,go-golang-org-x-term)
                         ("go-github-com-sirupsen-logrus" ,go-github-com-sirupsen-logrus)
                         ("go-github-com-shurcool-vfsgen" ,go-github-com-shurcool-vfsgen)
                         ("go-github-com-inconshreveable-mousetrap" ,go-github-com-inconshreveable-mousetrap)
                         ("go-golang-org-x-tools" ,go-golang-org-x-tools)
                         ("go-golang-org-x-sys" ,go-golang-org-x-sys)
                         ("go-golang-org-x-sync" ,go-golang-org-x-sync)
                         ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
                         ("go-github-com-spf13-pflag" ,go-github-com-spf13-pflag)
                         ("go-github-com-spf13-cobra" ,go-github-com-spf13-cobra)
                         ("go-github-com-shurcool-httpfs-filter"
                          ,go-github-com-shurcool-httpfs-filter)
                         ("go-github-com-shurcool-go-browser" ,go-github-com-shurcool-go-browser)
                         ("go-github-com-neelance-sourcemap" ,go-github-com-neelance-sourcemap)
                         ("go-github-com-neelance-astrewrite" ,go-github-com-neelance-astrewrite)
                         ("go-github-com-google-go-cmp" ,go-github-com-google-go-cmp)
                         ("go-github-com-fsnotify-fsnotify" ,go-github-com-fsnotify-fsnotify)))
    (home-page "https://github.com/gopherjs/gopherjs")
    (synopsis "GopherJS - A compiler from Go to JavaScript")
    (description
     "GopherJS compiles Go code (@@url{https://golang.org/,golang.org}) to pure
JavaScript code.  Its main purpose is to give you the opportunity to write
front-end code in Go which will still run in all browsers.")
    (license license:bsd-2)))

(define-public go-github-com-smartystreets-goconvey
  (package
    (name "go-github-com-smartystreets-goconvey")
    (version "1.8.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/smartystreets/goconvey")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fyg66mpza18f3x9inh9hnxnr86py83sqnav3s8airdf66nyh1r6"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/smartystreets/goconvey"
       #:go ,go-1.20))
    (propagated-inputs `(("go-golang-org-x-sys" ,go-golang-org-x-sys)
                         ("go-golang-org-x-mod" ,go-golang-org-x-mod)
                         ("go-github-com-gopherjs-gopherjs" ,go-github-com-gopherjs-gopherjs)
                         ("go-golang-org-x-tools" ,go-golang-org-x-tools)
                         ("go-github-com-smartystreets-assertions" ,go-github-com-smartystreets-assertions)
                         ("go-github-com-jtolds-gls" ,go-github-com-jtolds-gls)))
    (home-page "https://github.com/smartystreets/goconvey")
    (synopsis
     "SMARTY DISCLAIMER: Subject to the terms of the associated license agreement, this software is freely available for your use. This software is FREE, AS IN PUPPIES, and is a gift. Enjoy your new responsibility. This means that while we may consider enhancement requests, we may or may not choose to entertain requests at our sole and absolute discretion.")
    (description
     "This executable provides an HTTP server that watches for file system changes to
.go files within the working directory (and all nested go packages).  Navigating
to the configured host and port in a web browser will display the latest results
of running `go test` in each go package.")
    (license license:expat)))

(define-public go-github-com-insomniacslk-dhcp-dhcpv4
  (package
    (name "go-github-com-insomniacslk-dhcp-dhcpv4")
    (version "0.0.0-20230516061539-49801966e6cb")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/insomniacslk/dhcp")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1892h4svff3ffdj8qqxgvlsifm1pzngqgy0d5p2kmpyncvwgnf36"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/insomniacslk/dhcp/dhcpv4"
       #:unpack-path "github.com/insomniacslk/dhcp/"
       #:go ,go-1.20))
    (propagated-inputs `(("go-gopkg-in-yaml-v3" ,go-gopkg-in-yaml-v3)
                         ("go-golang-org-x-sync" ,go-golang-org-x-sync)
                         ("go-github-com-stretchr-objx" ,go-github-com-stretchr-objx)
                         ("go-github-com-smartystreets-goconvey" ,go-github-com-smartystreets-goconvey)
                         ("go-github-com-pmezard-go-difflib" ,go-github-com-pmezard-go-difflib)
                         ("go-github-com-pierrec-lz4-v4" ,go-github-com-pierrec-lz4-v4)
                         ("go-github-com-mdlayher-socket" ,go-github-com-mdlayher-socket)
                         ("go-github-com-josharian-native" ,go-github-com-josharian-native)
                         ("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)
                         ("go-golang-org-x-sys" ,go-golang-org-x-sys)
                         ("go-golang-org-x-net" ,go-golang-org-x-net)
                         ("go-github-com-u-root-uio-rand" ,go-github-com-u-root-uio-rand)
                         ("go-github-com-u-root-uio-uio" ,go-github-com-u-root-uio-uio)
                         ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
                         ("go-github-com-mdlayher-packet" ,go-github-com-mdlayher-packet)
                         ("go-github-com-mdlayher-netlink" ,go-github-com-mdlayher-netlink)
                         ("go-github-com-jsimonetti-rtnetlink" ,go-github-com-jsimonetti-rtnetlink)
                         ("go-github-com-hugelgupf-socketpair" ,go-github-com-hugelgupf-socketpair)
                         ("go-github-com-google-go-cmp" ,go-github-com-google-go-cmp)
                         ("go-github-com-fanliao-go-promise" ,go-github-com-fanliao-go-promise)))
    (home-page "https://github.com/insomniacslk/dhcp")
    (synopsis "dhcp")
    (description
     "DHCPv4 and DHCPv6 decoding/encoding library with client and server code, written
in Go.")
    (license license:bsd-3)))

(define-public go-github-com-insomniacslk-dhcp-iana
  (package
    (inherit go-github-com-insomniacslk-dhcp-dhcpv4)
    (name "go-github-com-insomniacslk-dhcp-iana")
    (arguments
     `(#:import-path "github.com/insomniacslk/dhcp/iana"
       #:unpack-path "github.com/insomniacslk/dhcp/"
       #:go ,go-1.20))))

(define-public go-github-com-insomniacslk-dhcp-interfaces
  (package
    (inherit go-github-com-insomniacslk-dhcp-dhcpv4)
    (name "go-github-com-insomniacslk-dhcp-interfaces")
    (arguments
     `(#:import-path "github.com/insomniacslk/dhcp/interfaces"
       #:unpack-path "github.com/insomniacslk/dhcp/"
       #:go ,go-1.20))))

(define-public go-github-com-insomniacslk-dhcp-rfc1035label
  (package
    (inherit go-github-com-insomniacslk-dhcp-interfaces)
    (name "go-github-com-insomniacslk-dhcp-rfc1035label")
    (arguments
     `(#:import-path "github.com/insomniacslk/dhcp/rfc1035label"
       #:unpack-path "github.com/insomniacslk/dhcp/"
       #:go ,go-1.20))))

(define-public go-github-com-mdlayher-netlink
  (package
    (name "go-github-com-mdlayher-netlink")
    (version "1.7.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mdlayher/netlink")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pxd0qn73jr9n64gkp2kd8q8x7xgssm3v8a68vkh88al55g8jkma"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/mdlayher/netlink"
       #:go ,go-1.20
       #:tests? #f))
    (propagated-inputs `(("go-golang-org-x-sync" ,go-golang-org-x-sync)
                         ("go-golang-org-x-sys" ,go-golang-org-x-sys)
                         ("go-golang-org-x-net" ,go-golang-org-x-net)
                         ("go-github-com-mdlayher-socket" ,go-github-com-mdlayher-socket)
                         ("go-github-com-josharian-native" ,go-github-com-josharian-native)
                         ("go-github-com-google-go-cmp" ,go-github-com-google-go-cmp)))
    (home-page "https://github.com/mdlayher/netlink")
    (synopsis "netlink")
    (description
     "Package netlink provides low-level access to Linux netlink sockets (AF_NETLINK).")
    (license license:expat)))

(define-public go-github-com-miekg-dns
  (package
    (name "go-github-com-miekg-dns")
    (version "1.1.54")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/miekg/dns")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bgk9h6w8h1d2c54lwidyxsc8kw1iadvvybrisjsqma5ds5pqhcl"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/miekg/dns"
       #:go ,go-1.20))
    (propagated-inputs `(("go-golang-org-x-mod" ,go-golang-org-x-mod)
                         ("go-golang-org-x-tools" ,go-golang-org-x-tools)
                         ("go-golang-org-x-sys" ,go-golang-org-x-sys)
                         ("go-golang-org-x-sync" ,go-golang-org-x-sync)
                         ("go-golang-org-x-net" ,go-golang-org-x-net)))
    (home-page "https://github.com/miekg/dns")
    (synopsis "Alternative (more granular) approach to a DNS library")
    (description
     "Package dns implements a full featured interface to the Domain Name System.
Both server- and client-side programming is supported.  The package allows
complete control over what is sent out to the DNS. The API follows the
less-is-more principle, by presenting a small, clean interface.")
    (license license:bsd-3)))

(define-public go-github-com-samber-lo
  (package
    (name "go-github-com-samber-lo")
    (version "1.38.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/samber/lo")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ilxcwgh22gqcrajvqz7jkw1z0h887dsxlisz8394q5zvckb0iiz"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/samber/lo"
       #:go ,go-1.20))
    (propagated-inputs `(("go-golang-org-x-exp" ,go-golang-org-x-exp)))
    (home-page "https://github.com/samber/lo")
    (synopsis "lo - Iterate over slices, maps, channels...")
    (description "✨")
    (license license:expat)))

(define-public go-github-com-vishvananda-netlink
  (let ((commit "55c8b9515a0162749068d94e83bbb71911aa50c3")
        (revision "0"))
    (package
      (name "go-github-com-vishvananda-netlink")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/vishvananda/netlink")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1w35ys08yr5riwwpy34b9k79qaf41zkpciypk9njmlrmnbwlahjk"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/vishvananda/netlink"
         #:go ,go-1.20
         #:tests? #f)) ; because it require root perm
      (propagated-inputs `(("go-golang-org-x-sys" ,go-golang-org-x-sys)
                           ("go-github-com-vishvananda-netns" ,go-github-com-vishvananda-netns)))
      (home-page "https://github.com/vishvananda/netlink")
      (synopsis "netlink - netlink library for go")
      (description
       "Package netlink provides a simple library for netlink.  Netlink is the interface
a user-space program in linux uses to communicate with the kernel.  It can be
used to add and remove interfaces, set up ip addresses and routes, and confiugre
ipsec.  Netlink communication requires elevated privileges, so in most cases
this code needs to be run as root.  The low level primitives for netlink are
contained in the nl subpackage.  This package attempts to provide a high-level
interface that is loosly modeled on the iproute2 cli.")
      (license license:asl2.0))))

(define-public go-github-com-prashantv-gostub
  (package
    (name "go-github-com-prashantv-gostub")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/prashantv/gostub")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "035xf5w4fqlicdbbjcflsqflc0z5gmrn6wr7q41xwqfwfpraf9ah"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/prashantv/gostub"))
    (propagated-inputs `(("go-gopkg-in-yaml-v3" ,go-gopkg-in-yaml-v3)
                         ("go-github-com-pmezard-go-difflib" ,go-github-com-pmezard-go-difflib)
                         ("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)
                         ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
    (home-page "https://github.com/prashantv/gostub")
    (synopsis "gostub")
    (description
     "Package gostub is used for stubbing variables in tests, and resetting the
original value once the test has been run.")
    (license license:expat)))

(define-public go-go-uber-org-automaxprocs
  (package
    (name "go-go-uber-org-automaxprocs")
    (version "1.5.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/uber-go/automaxprocs")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00k3c9zjka6ylxn5czljknaqjhia8js4qwz3z3mk1wllafb0kp77"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "go.uber.org/automaxprocs"))
    (propagated-inputs `(("go-gopkg-in-yaml-v3" ,go-gopkg-in-yaml-v3)
                         ("go-gopkg-in-check-v1" ,go-gopkg-in-check-v1)
                         ("go-github-com-pmezard-go-difflib" ,go-github-com-pmezard-go-difflib)
                         ("go-github-com-kr-pretty" ,go-github-com-kr-pretty)
                         ("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)
                         ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
                         ("go-github-com-prashantv-gostub" ,go-github-com-prashantv-gostub)))
    (home-page "https://go.uber.org/automaxprocs")
    (synopsis "automaxprocs")
    (description
     "Package automaxprocs automatically sets GOMAXPROCS to match the Linux container
CPU quota, if any.")
    (license license:expat)))

(define-public go-github-com-ajg-form
  (package
    (name "go-github-com-ajg-form")
    (version "1.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ajg/form")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1d6sxzzf9yycdf8jm5877y0khmhkmhxfw3sc4xpdcsrdlc7gqh5a"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/ajg/form"
       #:tests? #f))
    (home-page "https://github.com/ajg/form")
    (synopsis "form")
    (description "Package form implements encoding and decoding of
application/x-www-form-urlencoded data.")
    (license license:bsd-3)))

(define-public go-github-com-google-go-cmp
  (package
    (name "go-github-com-google-go-cmp")
    (version "0.5.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/go-cmp")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0a13m7l1jrysa7mrlmra8y7n83zcnb23yjyg3a609p8i9lxkh1wm"))))
    (build-system go-build-system)
    (arguments
     `(#:unpack-path "github.com/google/go-cmp"
       #:import-path "github.com/google/go-cmp/cmp/"))
    (home-page "https://github.com/google/go-cmp")
    (synopsis "Package for equality of Go values")
    (description
     "This package is intended to be a more powerful and safer alternative to
@@code{reflect.DeepEqual} for comparing whether two values are semantically
equal.")
    (license license:bsd-3)))

(define-public go-github-com-mdlayher-socket
  (package
    (name "go-github-com-mdlayher-socket")
    (version "0.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mdlayher/socket")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lb20cnlml61j4n666bbjpzaxb5r8c9w3xw534ax0g6zm9hnk17m"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/mdlayher/socket"
       #:go ,go-1.20))
    (propagated-inputs `(("go-golang-org-x-sys" ,go-golang-org-x-sys)
                         ("go-golang-org-x-sync" ,go-golang-org-x-sync)
                         ("go-golang-org-x-net" ,go-golang-org-x-net)
                         ("go-github-com-google-go-cmp" ,go-github-com-google-go-cmp)))
    (home-page "https://github.com/mdlayher/socket")
    (synopsis "socket")
    (description
     "Package socket provides a low-level network connection type which integrates
with Go's runtime network poller to provide asynchronous I/O and deadline
support.")
    (license license:expat)))

(define-public go-github-com-josharian-native
  (package
    (name "go-github-com-josharian-native")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/josharian/native")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wa4yzc3r06qjklqjf4n30zx9v660w8hmxkmybzwk03fmlv2rcyj"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/josharian/native"))
    (home-page "https://github.com/josharian/native")
    (synopsis #f)
    (description "Package native provides easy access to native byte order.")
    (license license:expat)))

(define-public go-github-com-pierrec-lz4-v4
  (package
    (name "go-github-com-pierrec-lz4-v4")
    (version "4.1.17")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pierrec/lz4")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bj9z8vsjq72rm0skp24q6ysj101kh1nc552ws038yqnb5fgbi26"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/pierrec/lz4/v4"
       #:tests? #f))
    (home-page "https://github.com/pierrec/lz4")
    (synopsis "lz4 : LZ4 compression in pure Go")
    (description
     "Package lz4 implements reading and writing lz4 compressed data.")
    (license license:bsd-3)))

(define-public go-github-com-u-root-uio-uio
  (package
    (name "go-github-com-u-root-uio-uio")
    (version "0.0.0-20230305220412-3e8cd9d6bf63")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/u-root/uio")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gvv66ixkgwikjx8sjdknvrmd08wv2ia02q8n8y3mnkhrgyyx1yf"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/u-root/uio/uio"
       #:unpack-path "github.com/u-root/uio"
       #:go ,go-1.20
       #:tests? #f))
    (propagated-inputs `(("go-golang-org-x-sys" ,go-golang-org-x-sys)
                         ("go-github-com-pierrec-lz4-v4" ,go-github-com-pierrec-lz4-v4)
                         ("go-github-com-josharian-native" ,go-github-com-josharian-native)))
    (home-page "https://github.com/u-root/uio")
    (synopsis "uio")
    (description #f)
    (license license:bsd-3)))

(define-public go-github-com-u-root-uio-rand
  (package
    (inherit go-github-com-u-root-uio-uio)
    (name "go-github-com-u-root-uio-rand")
    (arguments
     `(#:import-path "github.com/u-root/uio/rand"
       #:unpack-path "github.com/u-root/uio"
       #:go ,go-1.20
       #:tests? #f))))

(define-public go-github-com-vishvananda-netns
  (package
    (name "go-github-com-vishvananda-netns")
    (version "0.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vishvananda/netns")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rci8c211m57nya9il81fz6459pia3dj5i4b16fp34vjrkcxliml"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/vishvananda/netns"
       #:tests? #f))
    (propagated-inputs `(("go-golang-org-x-sys" ,go-golang-org-x-sys)))
    (home-page "https://github.com/vishvananda/netns")
    (synopsis "netns - network namespaces in go")
    (description
     "Package netns allows ultra-simple network namespace handling.  NsHandles can be
retrieved and set.  Note that the current namespace is thread local so actions
that set and reset namespaces should use LockOSThread to make sure the namespace
doesn't change due to a goroutine switch.  It is best to close NsHandles when
you are done with them.  This can be accomplished via a `defer ns.Close()` on
the handle.  Changing namespaces requires elevated privileges, so in most cases
this code needs to be run as root.")
    (license license:asl2.0)))

(define-public go-github-com-dreamacro-clash
  (package
    (name "go-github-com-dreamacro-clash")
    (version "1.16.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Dreamacro/clash")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zsyz1zqb886kjyin0m6b221djbq9z6m43mmricmqa2almhphrnb"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/Dreamacro/clash"
       #:go ,go-1.20
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'dont-symlink
           (lambda _
             (let* ((files
                     (list
                      "./src/golang.org/x/net/publicsuffix/data/children"
                      "./src/golang.org/x/net/publicsuffix/data/nodes"
                      "./src/golang.org/x/net/publicsuffix/data/text")))
               (map (lambda (filename)
                      (copy-file filename (string-append filename ".bak"))
                      (delete-file filename)
                      (rename-file (string-append filename ".bak") filename))
                    files)))))))
    (propagated-inputs `(("go-golang-org-x-tools" ,go-golang-org-x-tools)
                         ("go-golang-org-x-text" ,go-golang-org-x-text)
                         ("go-golang-org-x-mod" ,go-golang-org-x-mod)
                         ("go-golang-org-x-exp" ,go-golang-org-x-exp)
                         ("go-github-com-vishvananda-netns" ,go-github-com-vishvananda-netns)
                         ("go-github-com-u-root-uio-uio" ,go-github-com-u-root-uio-uio)
                         ("go-github-com-pmezard-go-difflib" ,go-github-com-pmezard-go-difflib)
                         ("go-github-com-pierrec-lz4-v4" ,go-github-com-pierrec-lz4-v4)
                         ("go-github-com-oschwald-maxminddb-golang"
                          ,go-github-com-oschwald-maxminddb-golang)
                         ("go-github-com-mdlayher-socket" ,go-github-com-mdlayher-socket)
                         ("go-github-com-kr-text" ,go-github-com-kr-text)
                         ("go-github-com-josharian-native" ,go-github-com-josharian-native)
                         ("go-github-com-google-go-cmp" ,go-github-com-google-go-cmp)
                         ("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)
                         ("go-github-com-ajg-form" ,go-github-com-ajg-form)
                         ("go-gopkg-in-yaml-v3" ,go-gopkg-in-yaml-v3)
                         ("go-golang-org-x-sys" ,go-golang-org-x-sys)
                         ("go-golang-org-x-sync" ,go-golang-org-x-sync)
                         ("go-golang-org-x-net" ,go-golang-org-x-net)
                         ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
                         ("go-go-uber-org-automaxprocs" ,go-go-uber-org-automaxprocs)
                         ("go-go-uber-org-atomic" ,go-go-uber-org-atomic)
                         ("go-go-etcd-io-bbolt" ,go-go-etcd-io-bbolt)
                         ("go-github-com-vishvananda-netlink" ,go-github-com-vishvananda-netlink)
                         ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
                         ("go-github-com-sirupsen-logrus" ,go-github-com-sirupsen-logrus)
                         ("go-github-com-samber-lo" ,go-github-com-samber-lo)
                         ("go-github-com-oschwald-geoip2-golang" ,go-github-com-oschwald-geoip2-golang)
                         ("go-github-com-miekg-dns" ,go-github-com-miekg-dns)
                         ("go-github-com-mdlayher-netlink" ,go-github-com-mdlayher-netlink)
                         ("go-github-com-insomniacslk-dhcp-dhcpv4"
                          ,go-github-com-insomniacslk-dhcp-dhcpv4)
                         ("go-github-com-insomniacslk-dhcp-iana"
                          ,go-github-com-insomniacslk-dhcp-iana)
                         ("go-github-com-insomniacslk-dhcp-interfaces"
                          ,go-github-com-insomniacslk-dhcp-interfaces)
                         ("go-github-com-insomniacslk-dhcp-rfc1035label"
                          ,go-github-com-insomniacslk-dhcp-rfc1035label)
                         ("go-github-com-gorilla-websocket" ,go-github-com-gorilla-websocket)
                         ("go-github-com-gofrs-uuid-v5" ,go-github-com-gofrs-uuid-v5)
                         ("go-github-com-go-chi-render" ,go-github-com-go-chi-render)
                         ("go-github-com-go-chi-cors" ,go-github-com-go-chi-cors)
                         ("go-github-com-go-chi-chi-v5" ,go-github-com-go-chi-chi-v5)
                         ("go-github-com-dreamacro-protobytes" ,go-github-com-dreamacro-protobytes)))
    (home-page "https://github.com/Dreamacro/clash")
    (synopsis "Features")
    (description
     "This is a general overview of the features that comes with Clash.")
    (license license:gpl3)))

go-github-com-dreamacro-clash
