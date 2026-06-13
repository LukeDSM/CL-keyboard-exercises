(defparameter database
  '((b1 shape brick)
    (b1 color green)
    (b1 size small)
    (b1 supported-by b2)
    (b1 supported-by b3)
    (b2 shape brick)
    (b2 color red)
    (b2 size small)
    (b2 supports b1)
    (b2 left-of b3)
    (b3 shape brick)
    (b3 color red)
    (b3 size small)
    (b3 supports b1)
    (b3 right-of b2)
    (b4 shape pyramid)
    (b4 color blue)
    (b4 size large)
    (b4 supported-by b5)
    (b5 shape cube)
    (b5 color green)
    (b5 size large)
    (b5 supports b4)
    (b6 shape brick)
    (b6 color purple)
    (b6 size large)))

#|
Write a function MATCH-ELEMENT that takes two symbols as in‐
puts. If the two are equal, or if the second is a question mark,
MATCH-ELEMENT should return T. Otherwise it should return
NIL. Thus (MATCH-ELEMENT ‘RED ‘RED) and (MATCH- EL‐
EMENT ‘RED ‘?) should return T, but (MATCH-ELEMENT
‘RED ‘BLUE) should return NIL. Make sure your function works
correctly before proceeding further.
|#

(defun match-element (x y)
  (or (eql x y)
      (eql x '?)
      (eql y '?)))

#|
Write a function MATCH-TRIPLE that takes an assertion and a
pattern as input, and returns T if the assertion matches the
pattern. Both inputs will be three-element lists.
(MATCH-TRIPLE ‘(B2 COLOR RED) ‘(B2 COLOR ?)) should return T.
(MATCH-TRIPLE ‘(B2 COLOR RED) ‘(Bl COLOR GREEN)) should return NIL.
Hint: Use MATCH-ELEMENT as a building block.
|#

(defun match-triple (assertation pattern)
  (and (match-element (first assertation) (first pattern))
       (match-element (second assertation) (second pattern))
       (match-element (third assertation) (third pattern))))

#|
Write the function FETCH that takes a pattern as input and returns
all assertions in the database that match the pattern. Remember that
DATABASE is a global variable. (FETCH ‘(B2 COLOR ?)) should
return ((B2 COLOR RED)), and (FETCH ‘(? SUPPORTS Bl))
should return ((B2 SUPPORTS Bl) (B3 SUPPORTS Bl)).
|#

(defun fetch (pattern)
  (remove-if-not
    (lambda (x)
      (match-triple x pattern))
    database))

#|
Write a function that takes a block name as input and returns a pat‐
tern asking the color of the block. For example, given the input B3,
your function should return the list (B3 COLOR ?).
|#

(defun get-color (block)
  `(,block color ?))

#|
Write a function SUPPORTERS that takes one input, a block, and
returns a list of the blocks that support it. (SUPPORTERS ‘Bl)
should return the list (B2 B3). Your function should work by con‐
structing a pattern containing the block’s name, using that pattern
as input to FETCH, and then extracting the block names from the
resulting list of assertions.
|#

(defun supporters (block)
  (mapcar #'first
    (fetch `(? supports ,block))))

#|
Write a predicate SUPP-CUBE that takes a block as input and re‐
turns true if that block is supported by a cube. (SUPP-CUBE ‘B4)
should return a true value; (SUPP-CUBE ‘Bl) should not because
Bl is supported by bricks but not cubes. Hint: Use the result of the
SUPPORTERS function as a starting point.
|#

(defun supp-cube (block)
  (loop for supporter in (supporters block)
        if (fetch `(,supporter shape cube))
          do (return t)))

#|
Write a function DESC1 that
takes a block as input and returns all assertions dealing with that block.
(DESC1 ‘B6) should return ((B6 SHAPE BRICK) (B6 COLOR PURPLE) (B6 SIZE LARGE)).
|#

(defun desc1 (block)
  (fetch `(,block ? ?)))

#|
Write a function DESC2 of one input that calls DESC1 and strips
the block name off each element of the result. (DESC2 ‘B6) should
return the list ((SHAPE BRICK) (COLOR PURPLE) (SIZE LARGE))
|#

(defun desc2 (cube)
  (mapcar #'rest (desc1 cube)))

#|
Write the DESCRIPTION function. It should take one input, call
DESC2, and merge the resulting list of lists into a single list.
(DESCRIPTION ‘B6) should return (SHAPE BRICK COLOR PURPLE SIZE LARGE).
|#

(defun description (cube)
  (reduce #'append (desc2 cube)))
