(in-package :prolog-user)

(<<-- male
  (damien) (david) (dustin) (michael) (richard))

(<<-- female
  (alicia) (betsy) (geraldine) (katie) (lisa) (mary) (teresa))

(<<-- (child geraldine)
  (alicia) (betsy) (david) (lisa) (michael) (richard) (teresa) (mary))

(<<- (child david)
  (damien) (dustin))

(<<- (child denise)
  (damien) (dustin))

(<-- (parent ?x ?y)
     (child ?x ?y))

(<-- (son ?parent ?child)
     (child ?parent ?child) (male ?child))

(<-- (daughter ?parent ?child)
     (child ?parent ?child) (female ?child))

(<-- (sibling ?x ?y)
     (parent ?parent ?x) (parent ?parent ?y) (not (== ?x ?y)))

(<-- (grandchild ?x ?y)
     (child ?x ?z) (child ?z ?y))

(<-- (grandson ?x ?y)
     (grandchild ?x ?y) (male ?y))

(<-- (granddaughter ?x ?y)
     (grandchild ?x ?y) (female ?y))
