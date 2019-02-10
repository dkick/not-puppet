;;;; Based on PAIP, Chapter 20, "Unification Grammars".

(in-package :com.github.dkick.mmsc-util.user)

(defmacro rule (head &optional (arrow ':-) &body body)
  (funcall (get arrow 'rule-function) head body))

(setf (get ':- 'rule-function)
      #'(lambda (head body) `(<- ,head ,body)))

(setf (get ':-- 'rule-function)
      #'(lambda (head body) `(<-- ,head ,body)))

(setf (get '--> 'rule-function)
      #'(lambda (head body) `(<- ,@(rewrite-dcg-term head body))))

(setf (get '---> 'rule-function)
      #'(lambda (head body) `(<-- ,@(rewrite-dcg-term head body))))

(defmacro ---> (head &body body)
  `(<-- ,@(rewrite-dcg-term head body)))

(defmacro --> (head &body body)
  `(<- ,@(rewrite-dcg-term head body)))

(defun dcg-normal-goal-p (x) (or (eq (car x) :test) (eq x '!)))

(defun dcg-word-list-p (x) (eq (car x) :word))

(defun rewrite-dcg-term (head body)
  (let* ((n (count-if (complement #'dcg-normal-goal-p) body))
         (symbols (use make-array (1+ n) :initial-contents
                       (loop repeat (1+ n) collect (gensym "?S/")))))
    (labels ((lemma (body n)
               (if (null body)
                   nil
                   (let ((goal (car body)))
                     (cond
                       ((eq goal '!) (cons '! (lemma (cdr body) n)))
                       ((dcg-normal-goal-p goal)
                        (append (cdr goal)
                                (lemma (cdr body) n)))
                       ((dcg-word-list-p goal)
                        (cons ` (= ,(aref symbols n)
                                   (,@(cdr goal) ,@(aref symbols (1+ n))))
                              (lemma (cdr body) (1+ n))))
                       (t (cons (append goal
                                        (list (aref symbols n)
                                              (aref symbols (1+ n))))
                                (lemma (cdr body) (1+ n)))))))))
      `((,@head ,(aref symbols 0) ,(aref symbols n))
        ,@(lemma body 0)))))
