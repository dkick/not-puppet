(in-package :com.github.dkick.mmsc-util.user)

(defparameter *sample-line*
  "12/31/2006 23:45:00.907  MAJOR      $CGI=        N:\\clearcase\\alisows1_view\\MMSC-1375.22\\mr\\mmsc\\lib/tcpcon.C:166 - TCP/IP Error: Connection refused for 127.0.0.1 on port 5132, Error:  4127, Result = -1")

(eval-when (:compile-toplevel :load-toplevel :execute)
(defvar *vob-tag* "N:\\clearcase\\alisows1_view\\MMSC-1375.22\\mr\\"))

(defun parse-log-file (file-namestring)
  (with-open-file (in file-namestring)
    (loop for line = (read-line in nil)
          for log-entry = (and line (parse-log-entry line))
          while line when log-entry nconc it)))

(defun parse-log-files (file-namestrings-list)
  (loop for file-namestring in file-namestrings-list
        nconc (parse-log-file file-namestring)))

(defun parse-log-file-directory (directory-namestring)
  (loop for pathname in (directory directory-namestring)
        when (equal (pathname-type pathname) "htm")
        nconc (parse-log-file pathname)))

(defun parse-log-entry (log-entry-string)
  (cl-ppcre:register-groups-bind
        (month day year hour minute second millisecond severity
         process-name file-and-maybe-skip-count line text)

      ;; Wow, this is fugly...
      ('(:sequence

         :start-anchor

         ;; month
         (:register (:sequence :digit-class :digit-class)) #\/
         ;; day
         (:register (:sequence :digit-class :digit-class)) #\/
         ;; year
         (:register
          (:sequence :digit-class :digit-class :digit-class :digit-class))

         #\Space

         ;; hour
         (:register (:sequence :digit-class :digit-class)) #\:
         ;; minute
         (:register (:sequence :digit-class :digit-class)) #\:
         ;; second
         (:register (:sequence :digit-class :digit-class)) #\.

         ;; millisecond
         (:register (:sequence :digit-class :digit-class :digit-class))

         (:greedy-repetition 1 nil :whitespace-char-class)

         ;; severity
         (:register (:greedy-repetition 1 nil :word-char-class))

         (:greedy-repetition 1 nil :whitespace-char-class)

         ;; process-name
         (:register
          (:sequence #\$
           (:greedy-repetition 1 nil
             (:char-class :word-char-class :digit-class #\=))))

         (:greedy-repetition 1 nil :whitespace-char-class)

         ;; file-and-maybe-skip-count
         (:register (:non-greedy-repetition 1 nil :everything))

         #\:

         ;; line
         (:register (:non-greedy-repetition 1 nil :digit-class))

         " - "

         ;; text
         (:register (:greedy-repetition 0 nil :everything))

         :end-anchor)

       log-entry-string)

    ;; I could probably collapse these two regular expressions into a
    ;; single one if I wanted to bother to do the work and test it.
    ;; However, this seems to work just fine at the moment.
    (cl-ppcre::register-groups-bind (count/string file)
        ('(:sequence
           :start-anchor
           (:greedy-repetition 0 1
            (:sequence
             "[Skipped "
             ;; count/string
             (:register (:greedy-repetition 1 nil :digit-class))
             "] "))
           ;; Don't bother to remember the VOB tag
           #.*vob-tag*
           ;; file
           (:register (:greedy-repetition 0 nil :everything))
           :end-anchor)
         file-and-maybe-skip-count)

      (loop repeat (if count/string
                       (1+ (parse-integer count/string))
                       1)
        collect (use make-log-entry/strings
                     month day year hour minute second millisecond
                     severity process-name file line text)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((load-truename *load-truename*))
    (defun log-entry-data-file ()
      (make-pathname :directory
                     (append (pathname-directory load-truename)
                             (list :up :up "Profiles" "DKICK1"
                                   "My Documents" "cnrc-case-nye"))
                     :name "sorted-event-log-data" :type "lisp"))))

(defvar *log-entry-data-file* (log-entry-data-file))

(defun load-log-entry-data-file (file-namestring)
  (with-open-file (in file-namestring)
    (loop for form = (read in nil) while form
          collect (apply #'make-log-entry form))))

(defclass log-entry ()
    ((time :initarg :time :reader log-entry-time)
     (severity :initarg :severity :reader log-entry-severity)
     (process-name :initarg :process-name :reader log-entry-process-name)
     (source-file :initarg :source-file :reader log-entry-source-file)
     (source-line :initarg :source-line :reader log-entry-source-line)
     (text :initarg :text :reader log-entry-text)))

(defun make-log-entry
    (time severity process-name source-file source-line text)
  (make-instance 'log-entry :time time :severity severity
                 :process-name process-name :source-file source-file
                 :source-line source-line :text text))

(defun write-log-entry-load-form (log-entry stream)
  (format stream "~&(make-instance 'log-entry ~
                     :time ~S :severity ~S :process-name ~S ~
                     :source-file ~S :source-line ~S :text ~S)~%"
          (log-entry-time log-entry)
          (log-entry-severity log-entry)
          (log-entry-process-name log-entry)
          (log-entry-source-file log-entry)
          (log-entry-source-line log-entry)
          (log-entry-text log-entry)))

(defun log-entry-as-list (log-entry)
  (list (log-entry-time log-entry)
        (log-entry-severity log-entry)
        (log-entry-process-name log-entry)
        (log-entry-source-file log-entry)
        (log-entry-source-line log-entry)
        (log-entry-text log-entry)))

(defmethod print-object ((object log-entry) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (prin1 (log-entry-time object) stream)
    (princ #\Space stream)
    (prin1 (log-entry-severity object) stream)
    (princ #\Space stream)
    (prin1 (log-entry-process-name object) stream)
    (princ #\Space stream)
    (prin1 (log-entry-source-file object) stream)
    (princ #\Space stream)
    (prin1 (log-entry-source-line object) stream)
    (princ #\Space stream)
    (prin1 (log-entry-text object) stream)))

(defconstant +milliseconds-per-second+ 1000)

(defun make-log-entry/strings
    (month day year hour minute second millisecond severity process-name
     file line text)
  (use make-instance 'log-entry
       :time (+ (* +milliseconds-per-second+
                   (use encode-universal-time
                        (parse-integer second) (parse-integer minute)
                        (parse-integer hour) (parse-integer day)
                        (parse-integer month) (parse-integer year)))
                (parse-integer millisecond))
       :severity severity :process-name process-name
       :source-file file :source-line (parse-integer line)
       :text text))

(defun log-entry-equal (x y)
  (and (equal (log-entry-time x) (log-entry-time y))
       (equal (log-entry-severity x) (log-entry-severity y))
       (equal (log-entry-process-name x) (log-entry-process-name y))
       (equal (log-entry-source-file x) (log-entry-source-file y))
       (equal (log-entry-source-line x) (log-entry-source-line y))
       (equal (log-entry-text x) (log-entry-text y))))

(defun make-event-log-type (log-entry)
  (list (log-entry-source-file log-entry)
        (log-entry-source-line log-entry)))

(defun set-event-log-types (hash log-entries)
  (clrhash hash)
  (loop for log-entry in log-entries
        for event-log-type = (make-event-log-type log-entry)
        do (setf (gethash event-log-type hash)
                 (push log-entry (gethash event-log-type hash)))
        finally (return hash)))

(defun max-event-log-types-list (hash)
  (let ((event-log-type nil) (max-length 0))
    (loop for key being each hash-key of hash
          using (hash-value value) do
          (let ((length (length value)))
            (when (> length max-length)
              (setq max-length length)
              (setq event-log-type key)))
          finally (return (values event-log-type max-length)))))

(defun sorted-event-log-types-alist (hash)
  (sort (loop for key being each hash-key of hash using (hash-value value)
              collect (cons key (length value)))
        #'> :key #'cdr))

(defun sorted-event-log-types (hash)
  (sort (loop for key being each hash-key of hash using (hash-value value)
              collect (list (length value) key
                            (log-entry-severity (car value))
                            (use remove-duplicates
                                 (mapcar #'log-entry-text value)
                                 :test #'string=)))
        #'> :key #'car))
