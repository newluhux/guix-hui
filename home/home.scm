(use-modules (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu services)
             (guix gexp))

(load "../manifest/x230.scm")

(home-environment
 (packages %x230-packages)
 (services
  (list
   (service home-bash-service-type
            (home-bash-configuration
             (guix-defaults? #t)
	     (aliases
	      '(("ustardict" . (string-append "ustardict "
					      "~/.guix-home/profile/share"
					      "/stardict/"
					      "stardict-ecdict-2.4.2."))))
             (bashrc (list (local-file ".bashrc" "bashrc")))))

   (simple-service 'my-environment-variables
                   home-environment-variables-service-type
                   `(("PAGER" . "less")
                     ("EDITOR" . "vi")
                     ("XMODIFIERS" . "@im=fcitx")
                     ("GTK_IM_MODULE" . "xim")
                     ("QT_IM_MODULE" . "xim")))

   (simple-service 'my-config-file
                   home-files-service-type
                   `((".emacs" ,(local-file ".emacs" "emacs"))
                     (".Xdefaults" ,(local-file ".Xdefaults" "Xdefaults"))
                     (".cwmrc" ,(local-file ".cwmrc" "cwmrc"))
                     (".xinitrc" ,(local-file ".xinitrc" "xinitrc"
                                             #:recursive? #t)))))))

