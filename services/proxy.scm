(use-modules
 (gnu services)
 (gnu services configuration)
 (gnu services shepherd)
 (gnu system shadow)
 ((gnu system file-systems) #:select (file-system-mapping))
 (gnu build linux-container)
 (guix least-authority)
 (guix gexp)
 (guix modules)
 (guix packages)
 (guix records)
 (ice-9 match))

(load "../packages/proxy.scm")

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
  (config-file clash-configuration-file
               (default #f)))

(define clash-shepherd-service
  (match-lambda
    (($ <clash-configuration> package config-file)
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
                           (source "/var/lib/clash")
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
                    `("-f" config-file)
                    '()))
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

(define-public clash-service-type
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
