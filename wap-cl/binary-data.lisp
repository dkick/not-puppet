(in-package :net.earthlink.dkixk.binary-data)

(defvar *in-progress-objects* nil)

(defconstant +null+ (code-char 0))

(defgeneric read-value (type stream &rest args &key)
  (:documentation "Read a value of the given type from the stream."))

(defgeneric write-value (type stream value &rest args &key)
  (:documentation "Write a value as the given type to the stream."))

(defgeneric read-object (object stream &key)
  (:method-combination + :most-specific-last)
  (:documentation "Fill in the slots of object from stream."))

(defgeneric write-object (object stream &key)
  (:method-combination progn :most-specific-last)
  (:documentation "Write out the slots of object to the stream."))

(defmethod read-value ((type symbol) stream &rest args &key)
  (let ((object (make-instance type)))
    (apply #'read-object object stream args)))

(defmethod write-value ((type symbol) stream value &rest args &key)
  (assert (typep value type))
  (apply #'write-object value stream args))

;;; Binary types

(defmacro define-binary-type (name (&rest args) &body spec)
  (with-gensyms (type stream value)
  `(progn
    (defmethod read-value ((,type (eql ',name)) ,stream &key ,@args)
      (declare (ignorable ,@args))
      ,(type-reader-body spec stream))
    (defmethod write-value ((,type (eql ',name)) ,stream ,value &key ,@args)
      (declare (ignorable ,@args))
      ,(type-writer-body spec stream value)))))

(defun type-reader-body (spec stream)
  (ecase (length spec)
    (1 (destructuring-bind (type &rest args) (mklist (first spec))
         `(read-value ',type ,stream ,@args)))
    (2 (destructuring-bind ((in) &body body) (cdr (assoc :reader spec))
         `(let ((,in ,stream)) ,@body)))))

(defun type-writer-body (spec stream value)
  (ecase (length spec)
    (1 (destructuring-bind (type &rest args) (mklist (first spec))
         `(write-value ',type ,stream ,value ,@args)))
    (2 (destructuring-bind ((out v) &body body) (cdr (assoc :writer spec))
         `(let ((,out ,stream) (,v ,value)) ,@body)))))

;;; Binary classes

(defmacro define-generic-binary-class
    (name (&rest superclasses) slots keywords &rest read-write-methods)
  `(progn
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (setf (get ',name 'slots) ',(mapcar #'first slots))
      (setf (get ',name 'keywords) ',keywords)
      (setf (get ',name 'superclasses) ',superclasses))

    (defclass ,name ,superclasses
        ,(mapcar #'slot->defclass-slot slots))

    ,@read-write-methods))

(defmacro define-binary-class
    (name (&rest superclasses-keywords) slots &rest options)
  (destructuring-bind (superclasses keywords)
      (split-on-key superclasses-keywords)
    (let ((tagged nil))
      `(define-generic-binary-class ,name ,superclasses ,slots ,keywords
        ,@(read-write-forms name superclasses keywords slots options
                            tagged)))))

(defmacro define-tagged-binary-class
    (name (&rest superclasses-keywords) slots &rest options)
  (destructuring-bind (superclasses keywords)
      (split-on-key superclasses-keywords)
    (let ((tagged t))
      `(define-generic-binary-class ,name ,superclasses ,slots ,keywords
        ,@(read-write-forms name superclasses keywords slots options
                            tagged)))))

(defun read-write-forms
    (name superclasses keywords slots options tagged)
  (mapcar #'(lambda (f)
              (funcall f name superclasses keywords slots options))
          (remove nil
                  (list (cond (tagged #'tagged-read-value-forms)
                              (keywords #'read-value-forms)
                              (t nil))
                        (when keywords #'write-value-forms)
                        (unless tagged #'read-object-forms)
                        #'write-object-forms))))

(defun read-object-forms (name superclasses keywords slots options)
  (let ((all-keywords (new-class-all-keywords keywords superclasses)))
    (with-gensyms (object stream)
      (let ((self-size (or (cadr (assoc :self-size options))
                           (make-symbol "SELF-SIZE"))))
        `(defmethod read-object + ((,object ,name) ,stream
                                   &key ,@all-keywords)
          (declare (ignorable ,stream ,@all-keywords))
          (let ((,self-size 0))
            (with-slots ,(new-class-all-slots slots superclasses)
                ,object
              ,@(mapcar #'(lambda (x)
                            (slot->read-value x stream self-size))
                        slots))
            ,self-size))))))

(defun write-object-forms (name superclasses keywords slots options)
  (declare (ignore options))
  (let ((all-keywords (new-class-all-keywords keywords superclasses)))
    (with-gensyms (object stream)
      `(defmethod write-object progn ((,object ,name) ,stream
                                      &key ,@all-keywords)
        (declare (ignorable ,stream ,@all-keywords))
        (with-slots ,(new-class-all-slots slots superclasses)
            ,object
          ,@(mapcar #'(lambda (x) (slot->write-value x stream))
                    slots))))))

(defun read-value-forms (name superclasses keywords slots options)
  (declare (ignore slots options))
  (let ((all-keywords (new-class-all-keywords keywords superclasses)))
    (with-gensyms (type object stream)
      `(defmethod read-value ((,type (eql ',name)) ,stream
                              &key ,@all-keywords)
        (let ((,object (make-instance ,type)))
          (read-object ,object ,stream
                       ,@(loop for s in keywords
                               collect (as-keyword s)
                               collect s)))))))

(defun write-value-forms (name superclasses keywords slots options)
  (declare (ignore slots options))
  (let ((all-keywords (new-class-all-keywords keywords superclasses)))
    (with-gensyms (type stream value)
      `(defmethod write-value ((,type (eql ',name)) ,stream ,value
                               &key ,@all-keywords)
        (assert (typep ,value ,type))
        (write-object ,value ,stream
                      ,@(loop for s in keywords
                              collect (as-keyword s)
                              collect s))))))

(defun tagged-read-value-forms (name superclasses keywords slots options)
  (let ((all-keywords (new-class-all-keywords keywords superclasses)))
    (destructuring-bind (&key dispatch self-size)
        (assoc :dispatch options)
      (unless dispatch (error "Must supply :dispatch form."))
      (with-gensyms
          (type object object-value object-size stream self-size-value)
        `(defmethod read-value ((,type (eql ',name)) ,stream
                                &key ,@all-keywords)
          (let* ((,self-size-value 0)
                 ,@(mapcar #'(lambda (x)
                               (slot->binding x stream self-size-value))
                           slots))
            (let ((,object (use make-instance ,dispatch
                                ,@(mapcan #'slot->keyword-arg slots))))
              (multiple-value-bind (,object-value ,object-size)
                  (use read-object ,object ,stream
                       ,@ (append (loop for s in keywords
                                        collect (as-keyword s)
                                        collect s)
                                  (when self-size
                                    (list self-size self-size-value))))
                (values ,object-value
                        (incf ,self-size-value ,object-size))))))))))

(defun split-on-key (list)
  ;; This will allow for (x y z &key) which seems to conform to what
  ;; defun and defgeneric/defmethod allow.
  (loop for (car . cdr) on list
        until (eql car '&key) collect car into classes
        finally (return (list classes cdr))))

(defun as-keyword (sym) (intern (string sym) :keyword))

(defun normalize-slot-spec (spec)
  (list (first spec) (mklist (second spec))))

(defun slot->defclass-slot (spec)
  (let ((name (first spec)))
    `(,name :initarg ,(as-keyword name) :accessor ,name)))

(defun slot->read-value (spec stream self-size)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    (with-gensyms (name-value name-size)
      `(multiple-value-bind (,name-value ,name-size)
           (read-value ',type ,stream ,@args)
         (setf ,name ,name-value)
         (incf ,self-size ,name-size)))))

(defun slot->write-value (spec stream)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    `(write-value ',type ,stream ,name ,@args)))

(defun slot->binding (spec stream self-size)
  (destructuring-bind (name (type &rest args)) (normalize-slot-spec spec)
    (with-gensyms (name-value name-size)
      `(,name (multiple-value-bind (,name-value ,name-size)
                  (read-value ',type ,stream ,@args)
                (incf ,self-size ,name-size)
                ,name-value)))))

(defun slot->keyword-arg (spec)
  (let ((name (first spec)))
    `(,(as-keyword name) ,name)))

;;; Keeping track of inherited slots

(defun direct-slots (name)
  (copy-list (get name 'slots)))

(defun inherited-slots (name)
  (loop for super in (get name 'superclasses)
        nconc (direct-slots super)
        nconc (inherited-slots super)))

(defun all-slots (name)
  (nconc (direct-slots name) (inherited-slots name)))

(defun direct-keywords (name)
  (copy-list (get name 'keywords)))

(defun inherited-keywords (name)
  (loop for super in (get name 'superclasses)
        nconc (direct-keywords super)
        nconc (inherited-keywords super)))

(defun all-keywords (name)
  (nconc (direct-keywords name) (inherited-keywords name)))

(defun new-class-all-slots (slots superclasses)
  "Like all slots but works while compiling a new class before slots
and superclasses have been saved."
  (nconc (mapcan #'all-slots superclasses) (mapcar #'first slots)))

(defun new-class-all-keywords (keywords superclasses)
  "Like all keywords but works while compiling a new class before
slots and superclasses have been saved."
  (nconc (mapcan #'all-keywords superclasses) (copy-list keywords)))

;;; In progress Object stack

(defun current-binary-object ()
  (first *in-progress-objects*))

(defun parent-of-type (type)
  (find-if #'(lambda (x) (typep x type)) *in-progress-objects*))

(defmethod read-object :around (object stream &key)
  (declare (ignore stream))
  (let* ((*in-progress-objects* (cons object *in-progress-objects*))
         (object-size (call-next-method)))
    (values object object-size)))

(defmethod write-object :around (object stream &key)
  (declare (ignore stream))
  (let ((*in-progress-objects* (cons object *in-progress-objects*)))
    (call-next-method)))

;;; Utilities

(defun swap-bytes (code)
  (assert (<= code #xffff))
  (rotatef (ldb (byte 8 0) code) (ldb (byte 8 8) code))
  code)
