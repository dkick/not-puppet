(in-package :prolog-user)

(defmacro <<-- (prefix/t &rest suffixes)
  (let ((prefix/list (mklist prefix/t)))
    `(progn
      (<-- ,(append prefix/list (car suffixes)))
      ,@(mapcar #'(lambda (suffix) `(<- ,(append prefix/list suffix)))
                (cdr suffixes)))))

(defmacro <<- (prefix/t &rest suffixes)
  (let ((prefix/list (mklist prefix/t)))
    `(progn
      ,@(mapcar #'(lambda (suffix) `(<- ,(append prefix/list suffix)))
                suffixes))))
