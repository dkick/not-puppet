(defpackage :com.github.dkick.mmsc-util.system
  (:use :cl :ul :asdf))

(in-package :com.github.dkick.mmsc-util.system)

(pushnew (make-pathname :host (pathname-host *load-truename*)
                        :device (pathname-device *load-truename*)
                        :directory (pathname-directory *load-truename*))
         *central-registry* :test #'equal)

(defsystem :mmsc-util
  :depends-on (:cl-ppcre :telnet-stream)
  :components
  ((:file "packages")
   (:file "generic-terminal-gf" :depends-on ("packages"))
   (:file "pathname-cygwin" :depends-on ("packages"))
   (:file "overrides" :depends-on ("packages" "pathname-cygwin"))
   (:file "generic-terminal"
          :depends-on ("packages" "pathname-cygwin" "overrides"
                       "generic-terminal-gf"))
   (:file "tacl-terminal"
          :depends-on ("packages" "generic-terminal-gf"
                       "generic-terminal" "overrides"))
   #+allegro
   (:file "ftp-terminal-allegro"
          :depends-on ("packages" "generic-terminal-gf"
                       "generic-terminal" "overrides"))
   #+clisp
   (:file "ftp-terminal-cl-ftp"
          :depends-on ("packages" "generic-terminal-gf"
                       "generic-terminal" "overrides"))
   (:file "bash-terminal"
          :depends-on ("packages" "generic-terminal-gf"
                       "generic-terminal"))
   (:file "osh-terminal"
          :depends-on ("packages" "generic-terminal-gf"
                       "generic-terminal"))
   (:file "misc"
          :depends-on ("packages" "pathname-cygwin" "overrides"
                       "generic-terminal-gf" "generic-terminal"))))
