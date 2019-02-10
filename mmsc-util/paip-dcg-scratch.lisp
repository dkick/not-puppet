;;;; So here is a little example of using Prolog's (or rather Prolog
;;;; in Lisp (sexp-logic)) Definitive Clause Grammar (DCG) to express
;;;; a tiny little subset of MM3.  The cool thing about DCGs is that
;;;; they can be used both to test whether or not a "sentence" is
;;;; acceptable according to the grammar as well as generate
;;;; "sentences" from the same grammar.

(in-package :com.github.dkick.mmsc-util.user)

(defvar *strict-eol* t
  "Whether or not to be strict about end-of-line being a carriage
return followed by a newline.")

;; Says that an MM3 message has a from, to, cc, subject, some other
;; MM3 headers, and a body.
(---> (mm3-message)
  (from) (to) (cc) (subject) (mm3-headers) (eol) (mm3-body) (eol))

;; From is the sequence "From: " followed by an address and
;; end-of-line.  The out of the box DCG only supports lists, as this
;; is what basic Prolog provides.  Expanding sexp-logic to allow for
;; "From: " to be a shorthand for (list #\F #\r #\o #\m #\:) would be
;; very easy.
(---> (from)
  (:word #\F #\r #\o #\m #\: #\Space) (address) (eol))

(---> (to)
  (:word #\T #\o #\: #\Space) (address) (eol))

;;; Cc can be either empty or "Cc: " follwed by an address and EOL.
(---> (cc))
(--> (cc)
  (:word #\C #\c \: #\Space) (address) (eol))

;;; Just use two fake addresses for this little demonstration.
(---> (address)
  (:word #\j #\o #\h #\n #\- #\d #\o #\e #\@ #\h #\e #\r #\e))
(--> (address)
  (:word #\j #\a #\n #\e #\- #\d #\o #\e #\@ #\t #\h #\e #\r #\e))

;; And a fake subject.
(---> (subject)
  (:word #\S #\u #\b #\j #\e #\c #\t #\: #\Space #\H #\e #\l #\l #\o)
  (eol))

;;; MM3 headers got to be a little more complicated to express the
;;; zero or more without any repeated headers.  Part of this could be
;;; my inexperience with DCGs, though.  There is probably a more
;;; succint way of expressing this.
(---> (mm3-headers))
(--> (mm3-headers)
  (mm3-headers-lemma () ?headers))
(---> (mm3-headers-lemma ?headers (?header . ?headers))
  (mm3-header ?header)
  (:test (not (member ?header ?headers))))
(--> (mm3-headers-lemma ?headers (?header/1 ?header/2 . ?headers))
  (mm3-headers-lemma ?headers (?header/1 . ?headers))
  (mm3-header ?header/2)
  (:test (not (= ?header/2 ?header/1)))
  (:test (not (member ?header/2 ?headers))))

;;; Just invent two fake headers for the purpose of demonstration.
(---> (mm3-header x-mms-foo)
  (:word #\X #\- #\m #\m #\s #\- #\f #\o #\o #\: #\Space #\b #\a #\r)
  (eol))
(--> (mm3-header x-mms-bar)
  (:word #\X #\- #\m #\m #\s #\- #\b #\a #\r #\: #\Space #\b #\a #\z)
  (eol))

;;; Blah blah blah, the body.
(---> (mm3-body)
  (:word #\b #\l #\a #\h #\Space #\b #\l #\a #\h #\Space #\b #\l #\a #\h))

;;; And the end-of-line for an example of restricting or expanding on
;;; the allowed "sentences".  Only allow a blank newline when we are
;;; not being strict about end-of-line.
(---> (eol)
  (:word #\Return #\Linefeed))
(--> (eol)
  (:test (lispp (not *strict-eol*)))
  (:word #\Newline))

;;; A quick exmaple of having lisp call out into the prolog and the
;;; prolog calling out into the lisp.
(defgeneric mm3-message-p (message))
(defmethod mm3-message-p ((message list))
  (prolog
    (lisp ?message message)
    (mm3-message ?message ())
    (lisp (return-from prolog ?message))))
(defmethod mm3-message-p ((message string))
  (mm3-message-p (coerce message 'list)))

(defun sample-mm3-message ()
  (flet ((eol () (eol-string)))
    (use concatenate 'string
         "From: john-doe@here" (eol) "To: jane-doe@there" (eol)
         "Subject: Hello" (eol) (eol) "blah blah blah" (eol))))

(defun sample-invalid-message ()
  (flet ((eol () (eol-string)))
    (use concatenate 'string
         "Form: john-doe@here" (eol) "To: jane-doe@there" (eol)
         "Subject: Hello" (eol) (eol) "blah blah blah" (eol))))

(defun eol-string ()
  (coerce (list #\Return #\Linefeed) 'string))
