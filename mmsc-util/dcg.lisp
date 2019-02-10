(in-package :com.github.dkick.mmsc-util.user)

(defmacro ---> (&rest term)
  `(<-- ,@(rewrite-dcg-term term)))

(defmacro --> (&rest term)
  `(<- ,@(rewrite-dcg-term term)))

(defun rewrite-dcg-term (term)
  (let ((arg-rest (gensym "?REST/")))
    (labels
        ((lemma (clause-1 clause-2 rest-clauses arg-1 arg-2 result)
           (cond
             ((null clause-1) result)
             ((dcg-nonterminal-p clause-2)
              (let ((next-clause-1 clause-2)
                    (next-clause-2 (car rest-clauses))
                    (next-rest-clauses (cdr rest-clauses)))
                (lemma next-clause-1 next-clause-2 next-rest-clauses
                       (next-arg-1 arg-1 arg-2)
                       (next-arg-2 rest-clauses)
                       (let ((arg-2/rest (or arg-2 arg-rest)))
                         (cons `(,@clause-1 ,arg-1 ,arg-2/rest)
                               result)))))
             ((dcg-terminal-p clause-2)
              (let ((next-clause-1 (car rest-clauses))
                    (next-clause-2 (cadr rest-clauses))
                    (next-rest-clauses (cddr rest-clauses)))
                (lemma next-clause-1 next-clause-2 next-rest-clauses
                       (next-arg-1 arg-1 arg-2)
                       (next-arg-2 next-clause-2)
                       (let ((arg-2/rest
                              (or (and rest-clauses arg-2)
                                  arg-rest)))
                         (cons `(,@clause-1 ,arg-1
                                 (,@(car clause-2) . ,arg-2/rest))
                               result)))))
             (t (error "~S is neither nonterminal nor terminal."
                       clause-2))))
         (next-arg-1 (arg-1 arg-2) (or arg-2 arg-1))
         (next-arg-2 (clause) (if clause (gensym "?LIST/") arg-rest)))
      (when (dcg-terminal-p (car term))
        (error "DCG term ~S must start with a nonterminal clause.  ~
                Head clause is terminal ~S."
               term (car term)))
      (destructuring-bind (clause-1 clause-2 &rest rest-clauses) term
        (if (and (dcg-nonterminal-p clause-1) (dcg-terminal-p clause-2)
                 (null rest-clauses))
            `((,@clause-1 (,@(car clause-2) . ,arg-rest) ,arg-rest))
            (use nreverse
                 (lemma clause-1 clause-2 rest-clauses
                        (gensym "?LIST/") nil
                        '())))))))

(defun dcg-terminal-p (x)
  "Corresponds to < x > in BNF.  [x] in Prolog.  ((x)) in sexp
Prolog."
  (and (listp x) (listp (car x)) (null (cdr x))
       (every #'atom (car x))))

(defun dcg-nonterminal-p (x)
  (and (listp x) (symbolp (car x))))

(defmacro dcg-test (dcg-term rewrite-term)
  (with-gensyms (terms ?terms)
    ` (cons ',dcg-term
            #'(lambda (,terms)
                (prolog
                  (lisp ,?terms ,terms)
                  (= ,?terms ,rewrite-term)
                  (lisp (return-from prolog ',rewrite-term)))))))

(defmacro define-dcg-tests (test-fn tests-var &body forms)
  (with-gensyms
      (tests dcg-term expect-fn term-rewrite result passes fails)
    ` (progn
        (defparameter ,tests-var
          (list ,@(mapcar #'(lambda (form)
                              `(dcg-test ,(car form) ,(cadr form)))
                          forms)))
        (defun ,test-fn (&optional (,tests ,tests-var))
          (loop for (,dcg-term . ,expect-fn) in ,tests
                for ,term-rewrite = (rewrite-dcg-term ,dcg-term)
                for ,result = (cons ,dcg-term ,term-rewrite)
                if (funcall ,expect-fn ,term-rewrite)
                collect ,result into ,passes
                else collect ,result into ,fails
                finally (return (values ,fails ,passes)))))))

(define-dcg-tests test-rewrite-dcg-term *test-rewrite-dcg-term-data*
  (((move) (step))
   ((move ?list/1 ?rest)
    (step ?list/1 ?rest)))
  (((move) (step) (move))
   ((move ?list/1 ?rest)
    (step ?list/1 ?list/2)
    (move ?list/2 ?rest)))
  (((step) ((up)))
   ((step (up . ?rest) ?rest)))
  (((step) ((down)))
   ((step (down . ?rest) ?rest)))
  ;; What about ((n) (n1) ([] t2) (n3) ([] t4))
  ;;            ((n) (n1) ([] t2) (n3) ([] t4) ({} do-stuff))
  ;; As an alternative syntax?
  (((n) (n1) ((t2)) (n3) ((t4)))
   ((n ?list/1 ?rest)
    (n1 ?list/1 (t2 . ?list/2))
    (n3 ?list/2 (t4 . ?rest))))
  ;;; Not yet sure about the macroexpansions of the following as I
  ;;; don't find example of the rewrite from DCG terms to simple
  ;;; Prolog terms in the book.
  (((s) ((a)) ((b)))
   ((s (a b . ?rest) ?rest)))
  (((s) ((a)) (s) ((b)))
   ((s (a . ?list/1) ?rest)
    (s ?list/1 (b . ?rest)))))
