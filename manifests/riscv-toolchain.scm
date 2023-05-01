(use-modules (guix profiles))

(load "../packages/embedded.scm")
(define-public %riscv-toolchain
  (list
   riscv64-unknown-elf-toolchain
   riscv64-unknown-elf-gdb))

(packages->manifest
 %riscv-toolchain)
