(in-package :com.github.dkick.mmsc-util)

(defgeneric make-terminal (shell stream &key))
(defgeneric make-subterminal (shell supershell stream &key))
(defgeneric init-subshell (shell supershell stream &key))

(defgeneric cmd-do (terminal cmd-what))
(defgeneric cmd-do* (shell stream cmd-what))

(defgeneric cmd-exit (terminal))
(defgeneric cmd-exit* (shell stream))
(defgeneric cmd-exit-close-stream (stream))
(defgeneric cmd-exit-subshell* (shell supershell stream))

(defgeneric cmd-send-execs (terminal execs))
(defgeneric cmd-put-exec (terminal exec-dir exec-name))
(defgeneric cmd-put-exec* (shell stream exec-dir exec-name))

(defgeneric cmd-sql-compiles (terminal execs))
(defgeneric cmd-cd (terminal path))
(defgeneric cmd-cd* (shell stream path))
(defgeneric ensure-path (shell path))
(defgeneric cmd-cd-volume-tag (terminal volume tag))
(defgeneric cmd-cd-volume-tag* (shell stream volume tag))
(defgeneric cmd-set-param (terminal param-name param-value))

(defgeneric cmd-backup-mmexec (terminal execs))

(defgeneric cmd-move-execs-to-mmexec (terminal execs))
(defgeneric cmd-move-exec-to-mmexec (terminal exec))

(defgeneric cmd-restart-web-server (terminal execs &key start-osh))

(defgeneric cmd-stop-mmexec-start-execs (terminal))
(defgeneric cmd-wait-for-delmgr (terminal volume))
(defgeneric cmd-wait-for-delmgr* (shell stream volume))

(defgeneric cmd-move-execs-from-mmexec (terminal execs))

(defgeneric cmd-restore-mmexec (terminal execs))

(defgeneric cmd-stop-execs (terminal))

(defgeneric shell-prompt (shell))
(defgeneric exec-backup-name (shell exec-name))
