(use-modules
 (gnu home)
 (gnu home services)
 (gnu home services shells)
 (gnu services)
 (guix gexp))

(load "../manifest/x230.scm")

(home-environment
 (packages %x230-packages)
 (services
  (list
   (service home-zsh-service-type
            (home-zsh-configuration
             (environment-variables
              '(("PAGER" . "less")
                ("EDITOR" . "emacsclient -c")
                ("XMODIFIERS" . "@im=fcitx")
                ("GTK_IM_MODULE" . "xim")
                ("QT_IM_MODULE" . "xim")))
             (zshrc
              (list (local-file ".zshrc" "zshrc")))))

   (simple-service 'my-config-file
                   home-files-service-type
                   `((".emacs" ,(local-file ".emacs" "emacs"))
                     (".Xdefaults" ,(local-file ".Xdefaults" "Xdefaults"))
                     (".cwmrc" ,(local-file ".cwmrc" "cwmrc"))
                     (".clang-format" ,(local-file ".clang-format"
						   "clang-format"))
                     (".xinitrc" ,(local-file ".xinitrc" "xinitrc"
                                              #:recursive? #t)))))))

