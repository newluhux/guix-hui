(define-module (hui services games)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages minetest)
  #:use-module (gnu system shadow)
  #:use-module ((gnu system file-systems) #:select (file-system-mapping))
  #:use-module (gnu build linux-container)
  #:autoload   (guix least-authority) (least-authority-wrapper)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (minetest-configuration
            minetest-configuration?
            minetest-service-type))

(define %minetest-accounts
  (list
   (user-account
    (name "minetest")
    (group "minetest")
    (system? #t)
    (comment "minetest daemon user")
    (home-directory "/var/lib/minetest/")
    (shell (file-append shadow "/sbin/nologin")))
   (user-group
    (name "minetest")
    (system? #t))))

(define-record-type* <minetest-configuration>
  minetest-configuration make-minetest-configuration
  minetest-configuration?
  (package minetest-configuration-minetest
           (default minetest))
  (plugins minetest-configuration-plugins
           (default (list minetest)))
  (data-dir minetest-configuration-data-dir
            (default "/var/lib/minetest/")))

(define minetest-shepherd-service
  (match-lambda
    (($ <minetest-configuration> package plugins data-dir)
     (shepherd-service
      (documentation "minetest daemon")
      (provision
       (list
        (symbol-append
         'minetest- (string->symbol data-dir))))
      (requirement '(networking))
      (start
       #~(make-forkexec-constructor
          (list #$(file-append package "/bin/minetest") "--server")
          #:environment-variables
          (list (string-append "HOME=" #$data-dir)
                (string-append
                 "MINETEST_SUBGAME_PATH="
                 "/run/current-system/profile/share/minetest/games")
                (string-append
                 "MINETEST_MOD_PATH="
                 "/run/current-system/profile/share/minetest/mods"))
          #:user "minetest" #:group "minetest"))
      (stop #~(make-kill-destructor))
      (respawn? #t)))))

(define minetest-activation
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        (let* ((user (getpw "minetest"))
               (directory "/var/lib/minetest/"))
          (mkdir-p directory)
          (chown directory (passwd:uid user) (passwd:gid user))))))

(define minetest-service-type
  (service-type
   (name 'minetest)
   (description "minetest daemon")
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list minetest-shepherd-service))
          (service-extension account-service-type
                             (const %minetest-accounts))
          (service-extension activation-service-type
                             (const minetest-activation))
          (service-extension profile-service-type
                             minetest-configuration-plugins)))
   (default-value (minetest-configuration))))
