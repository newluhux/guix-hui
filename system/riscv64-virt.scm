(use-modules (gnu))
(use-package-modules linux)

(load "./bare.scm")

(define-public riscv64-virt-bare-os
 (operating-system
  (inherit bare-os)
  (kernel linux-libre-riscv64-generic)))

riscv64-virt-bare-os
