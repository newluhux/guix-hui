(define-module (hui services proxy)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages admin)
  #:use-module (hui packages proxy)
  #:use-module (gnu system shadow)
  #:use-module ((gnu system file-systems) #:select (file-system-mapping))
  #:use-module (gnu build linux-container)
  #:autoload   (guix least-authority) (least-authority-wrapper)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (clash-configuration
            clash-configuration?
            clash-service-type))

(define %clash-accounts
  (list
   (user-account
    (name "clash")
    (group "clash")
    (system? #t)
    (comment "clash daemon user")
    (home-directory "/var/lib/clash/")
    (shell (file-append shadow "/sbin/nologin")))
   (user-group
    (name "clash")
    (system? #t))))

(define-record-type* <clash-configuration>
  clash-configuration make-clash-configuration
  clash-configuration?
  (package clash-configuration-clash
           (default go-github-com-dreamacro-clash))
  (config-file clash-configuration-config-file
               (default #f))
  (data-dir clash-configuration-data-dir
            (default "/var/lib/clash/")))

(define clash-shepherd-service
  (match-lambda
    (($ <clash-configuration> package config-file data-dir)
     (let ((clash
            (least-authority-wrapper
             (file-append package "/bin/clash")
             #:name "clash"
             #:mappings (append
                         (if config-file
                             (list
                              (file-system-mapping
                               (source config-file)
                               (target source)
                               (writable? #f)))
                             (list))
                         (list
                          (file-system-mapping
                           (source data-dir)
                           (target source)
                           (writable? #t))))
             #:namespaces
             (delq 'net %namespaces))))
       (shepherd-service
        (documentation "clash daemon")
        (provision
         (list
          (symbol-append
           'clash- (if config-file
                       (string->symbol config-file)
                       (string->symbol "default")))))
        (requirement '(networking))
        (start
         #~(make-forkexec-constructor
            (list
             #$clash
             #$@(if config-file
                    (list "-f" config-file)
                    (list))
             "-d" #$data-dir)
            #:user "clash" #:group "clash"))
        (stop #~(make-kill-destructor))
        (respawn? #t))))))

(define clash-activation
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        (let* ((user (getpw "clash"))
               (directory "/var/lib/clash/"))
          (mkdir-p directory)
          (chown directory (passwd:uid user) (passwd:gid user))))))

(define clash-service-type
  (service-type
   (name 'clash)
   (description "clash daemon")
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list clash-shepherd-service))
          (service-extension account-service-type
                             (const %clash-accounts))
          (service-extension activation-service-type
                             (const clash-activation))))
   (default-value (clash-configuration))))
