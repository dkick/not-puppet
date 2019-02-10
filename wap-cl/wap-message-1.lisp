;;; Or maybe this one...

(define-binary-class message-entity (&key length)
  ((headers message-headers)
   (body something-to-get-a-body)))

(define-binary-class content-entity ()
  ((headers content-headers)
   (body something-to-get-a-body)))
