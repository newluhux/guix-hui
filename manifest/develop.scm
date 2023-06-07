(use-modules (gnu) (guix profiles))
(use-package-modules
 man linux elf code version-control
 android admin build-tools cmake haskell-apps
 java ccache file hexedit bison flex
 bootloaders llvm wine license disk)

(load "../packages/embedded.scm")
(load "../packages/engineering.scm")

(define-public %develop-packages
  (list
   (list glibc "static") mandoc man-pages
   linux-libre-headers patchelf
   gdb clang-toolchain cscope indent
   git gnu-make adb fastboot xfel shellcheck
   openocd blisp bear openixcard
   python-extract-dtb cmake cutter-rizin
   rizin (list openjdk "jdk") abootimg
   binwalk strace ltrace perf ccache file
   bvi bison flex dtc u-boot-tools pluseview
   ufbterm wine64 licensecheck mtd-utils bmaptools))

(packages->manifest %develop-packages)
