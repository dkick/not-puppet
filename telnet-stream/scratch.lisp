(in-package :net.earthlink.dkixk.telnet-stream.user)

(defun duff-test ()
  (with-telnet (server *hostname*)
    (labels
        ;; I'm calling this %read-char and %write-char to emphasize
        ;; that I would like recv-char to really be read-char working
        ;; with a telnet-stream.  They both echo.
        ((%read-char ()
           (prog1 (princ (recv-char server)) (finish-output)))
         (%write-char (char) (send-char char server))
         (%wait-for (expected)
           (loop with i = 0
                 for c = (%read-char)
                 if (eql c (schar expected i)) do (incf i)
                 else do (setq i 0)
                 while (< i (length expected))))
         (%writeln (string)
           (loop for c across string do (%write-char c))
           (%write-char #\Return)
           (%write-char #\Linefeed)
           (force-output (..::tcp-stream server))))
      (%wait-for "Enter Choice> ")
      (%writeln "TACL")
      (%wait-for "TACL 1> ")
      (%writeln (concatenate 'string "logon " *username*))
      (%wait-for "Password: ")
      (%writeln *password*)
      (%wait-for "$APPL1 MGR 1> ")
      (%writeln "fup vols")
      (%wait-for "$APPL1 MGR 2> "))))
