(use-modules (guix profiles))
(use-package-modules embedded)

(define-public %arm-toolchain
  (list
   arm-none-eabi-nano-toolchain-7-2018-q2-update
   gdb-arm-none-eabi))

(packages->manifest
 %arm-toolchain)
