(use-modules (gnu) (guix profiles))
(use-package-modules
 man linux elf code version-control
 android admin build-tools cmake
 java ccache file hexedit bison flex
 bootloaders)

(load "../packages/embedded.scm")
(load "../packages/engineering.scm")

(define-public %develop-packages
  (list
   (list glibc "static") mandoc man-pages
   linux-libre-headers patchelf
   gdb gcc-toolchain cscope indent
   git gnu-make adb fastboot xfel
   openocd blisp bear ccls openixcard
   python-extract-dtb cmake cutter-rizin
   rizin (list openjdk "jdk") abootimg
   binwalk strace ltrace perf ccache file
   bvi bison flex dtc u-boot-tools pluseview))

(packages->manifest %develop-packages)
