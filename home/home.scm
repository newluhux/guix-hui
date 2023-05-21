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
             (bashrc (list (local-file ".bashrc" "bashrc")))))

   (simple-service 'my-environment-variables
                   home-environment-variables-service-type
                   `(("PAGER" . "less")
                     ("EDITOR" . "emacsclient")))

   (simple-service 'my-config-file
                   home-files-service-type
                   `(("emacs" ,(local-file ".emacs" "emacs"))
                     ("Xdefaults" ,(local-file ".Xdefaults" "Xdefaults"))
                     ("cwmrc" ,(local-file ".cwmrc" "cwmrc"))
                     ("xinitrc" ,(local-file ".xinitrc" "xinitrc"
                                             #:recursive? #t)))))))

