(in-package :cl-user)

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :ftp)
  (require :osi)
  (require :prolog)
  (require :socket))

(defpackage :com.github.dkick.mmsc-util
  (:use :cl :ul :prolog :prolog-ext :net.ftp.client :net.uri :cl-ppcre
        :net.earthlink.dkixk.telnet-stream)
  (:export
   ;; Symbols from pathname-cygwin.lisp
   :make-pathname/cygwin :pathname/cygwin :pathname-directory/cygwin
   :pathname-name/cygwin :pathname-type/cygwin
   ;; Symbols from overrides.lisp
   :*execs* :*overrides* :*default-host-info-username*
   :*default-host-info-password* :*default-super-super-password*
   :*default-super-webmastr-password* :*default-overrides-volume*
   :make-overrides :read-overrides :make-host-info :vob-tag
   :snet-host-info :snet-hostname :snet-username :snet-volume :snet-tag
   :exec-directory-pathname
   ;; The following symbols are used with objects of typep
   ;; generic-shell-terminal.
   :with-terminal :with-terminals
   :make-terminal :cmd-do :cmd-exit :cmd-send-execs :cmd-put-exec
   :cmd-sql-compiles :cmd-cd-volume-tag :cmd-set-param
   :cmd-backup-mmexec :cmd-move-execs-to-mmexec :cmd-move-exec-to-mmexec
   :cmd-restart-web-server :cmd-stop-mmexec-start-execs
   :cmd-wait-for-delmgr :cmd-move-execs-from-mmexec :cmd-restore-mmexec
   :cmd-stop-execs
   ;; The following symbols are used as arguments to make-terminal to
   ;; create a particular subtype of generic-shell-terminal.
   :tacl-shell :ftp-shell :bash-shell :telnet
   ;; From misc.lisp.  The following function will soon be replaced by
   ;; something from generic-terminal-shell.lisp.
   :write-p2t-setup-cmds :remote-p2t-setup :remote-p2t-restore
   :remote-p2t-put-execs-to-rdkexe :remote-p2t-setup-from-rdkexe
   :set-overrides-for-remote-p2t-setup))

(defpackage :com.github.dkick.mmsc-util.user
  (:use :cl :ul :prolog :prolog-ext :net.uri
        :com.github.dkick.mmsc-util
        :net.earthlink.dkixk.telnet-stream ))
