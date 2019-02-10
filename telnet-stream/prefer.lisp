;;; Prefer is like Don Libe's Expect, only with a very very very much
;;; reduced feature set.

(in-package :net.earthlink.dkixk.telnet-stream)

(defvar *default-echo-fn* #'princ)
(defvar *default-recv-char-fn* nil)

(defun wait-for (what where &key
                      (echo-fn *default-echo-fn*)
                      (recv-char-fn *default-recv-char-fn*))
  (wait-for* what where :echo-fn echo-fn :recv-char-fn recv-char-fn))

(defun send-line (what where &key (use-force-output t))
  (send-line* what where :use-force-output use-force-output))

(defgeneric wait-for* (what where &key echo-fn recv-char-fn))
(defgeneric send-line* (what where &key use-force-output))

(defmethod wait-for* ((what string) (where generic-telnet-stream)
                      &key echo-fn recv-char-fn)
  (wait-for* (list :sequence what) where
             :echo-fn echo-fn :recv-char-fn recv-char-fn))

(define-condition mismatch-during-wait-for (simple-condition) ())

(defmethod wait-for* ((what list) (where generic-telnet-stream)
                      &key echo-fn recv-char-fn)
  (assert (eq (car what) :sequence))
  (let ((peek nil))
    (labels ((recv-char* ()
               (cond (peek (prog1 peek (setq peek nil)))
                     (t (let ((char (recv-char where)))
                          (prog1 char
                            (when recv-char-fn
                              (funcall recv-char-fn char))
                            (when echo-fn
                              (funcall echo-fn char)))))))
             (string* (expected)
               (loop with i = 0
                     for c = (recv-char*)
                     if (eql c (schar expected i)) do (incf i)
                     else do (signal 'mismatch-during-wait-for)
                     while (< i (length expected))))
             (digits ()
               (unless (digit-char-p (recv-char*))
                 (signal 'mismatch-during-wait-for))
               (loop for c = (recv-char*)
                     while (digit-char-p c)
                     finally (progn
                               (assert (null peek))
                               (setq peek c))))
             (lemma (list)
               (handler-case
                   (loop for element in list do
                         (cond ((typep element 'string) (string* element))
                               ((eq element :digits) (digits))
                               (t (error "Unknown type of element: ~S."
                                         element)))
                         finally (return t))
                 (mismatch-during-wait-for () nil))))
      (loop until (lemma (cdr what))))))

(defmethod send-line* ((what t) (where telnet-tcp-stream-mixin)
                       &key use-force-output)
  (loop for c across what do (send-char c where))
  (send-char #\Return where)
  (send-char #\Linefeed where)
  (when use-force-output
    (force-output (telnet-tcp-stream where))))
