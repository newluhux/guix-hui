(use-modules (guix profiles) (hui packages embedded))

(define-public %riscv-toolchain
  (list
   riscv64-unknown-elf-toolchain
   riscv64-unknown-elf-gdb))

(packages->manifest
 %riscv-toolchain)
