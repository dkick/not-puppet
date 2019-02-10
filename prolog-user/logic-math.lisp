(in-package :prolog-user)

(<-- (natural-number 0))
(<- (natural-number (s ?x))
    (natural-number ?x))

(<-- (count-s 0 0))
(<- (count-s (s ?x) ?d)
    (count-s ?x ?d1)
    (is ?d (1+ ?d1)))
