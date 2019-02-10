(in-package :prolog-user)

;; The valid values for the parameters.
(<<-- (param msv-numbering-plan)
  (simple) (domestic))
(<<- (param msv-authentication-sequence)
  ((static)) ((static dynamic)) ((dynamic static)))

;; Given the ?name of a parameter, it's current ?value, and the value
;; we ?expect, ?command will make the change.
(<-- (param ?name ?value ?expect ?command)
     (param ?name ?value) (param ?name ?expect)
     (= ?command (set ?name ?expect)) (not (= ?value ?expect)))
;; Given the ?name of a parameter, it's current ?value, and this is
;; exactly what we ?expect, ?command will be (), to represent that
;; nothing needs to be done.
(<- (param ?name ?value ?expect ?command)
    (param ?name ?value) (param ?name ?expect)
    (= ?command ()) (= ?value ?expect))

;; I'm sure there is a better way to do this, probably something to do
;; with using a list of parameters instead of explicit variables, but
;; I'm still learning big chunks of prolog.  And so, for now, I'll
;; play with the lispness of prolog in lisp.
(defmacro <--/msv-config (&rest parameters)
  (macrolet ((fn (&rest args)
               ` (mapcar #'(lambda (name)
                             (with-gensyms (,@args)
                               (list 'param name ,@args)))
                         parameters)))
    (let ((type (fn ?value))
          (cmds (fn ?value ?expect ?command)))
      ` (progn
          (<-- (msv-config ,@type) ,@type)
          (<- (msv-config ,@cmds) ,@cmds)))))

(<--/msv-config msv-numbering-plan msv-authentication-sequence)
