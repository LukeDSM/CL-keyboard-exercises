(defparameter rooms
  '((living-room
      (north front-stairs)
      (south dining-room)
      (east kitchen))
    (upstairs-bedroom
      (west library)
      (south front-stairs))
    (dining-room
      (north living-room)
      (east pantry)
      (west downstairs-bedroom))
    (kitchen
      (west living-room)
      (south pantry))
    (pantry
      (north kitchen)
      (west dining-room))
    (downstairs-bedroom
      (north back-stairs)
      (east dining-room))
    (back-stairs
      (south downstairs-bedroom)
      (north library))
    (front-stairs
      (north upstairs-bedroom)
      (south living-room))
    (library
      (east upstairs-bedroom)
      (south back-stairs))))

#|
Write a function CHOICES that takes the name of a room as input
and returns the table of permissible directions Robbie may take
from that room. For example, (CHOICES ‘PANTRY) should return
the list ((NORTH KITCHEN) (WEST DINING-ROOM)). Test
your function to make sure it returns the correct result.
|#

(defun choices (room)
  (rest (assoc room rooms)))

#|
Write a function LOOK that takes two inputs, a direction and a
room, and tells where Robbie would end up if he moved in that di‐
rection from that room. For example, (LOOK ‘NORTH ‘PANTRY)
should return KITCHEN. (LOOK ‘WEST ‘PANTRY) should re‐
turn DINING-ROOM. (LOOK ‘SOUTH ‘PANTRY) should return
NIL. Hint: The CHOICES function will be a useful building block.
|#

(defun look (dir room)
  (cadr (assoc dir (choices room))))

#|
We will use the global variable LOC to hold Robbie’s location.
Type in an expression to set his location to be the pantry. The
following function should be used whenever you want to change his
location.
|#

(defvar loc 'pantry)

(defun set-robbie-location (place)
  (setf loc place))

#|
Write a function HOW-MANY-CHOICES that tells how many
choices Robbie has for where to move to next. Your function
should refer to the global variable LOC to find his current location.
If he is in the pantry, (HOW-MANY-CHOICES) should return 2.
|#

(defun how-many-choices ()
  (length (choices loc)))

#|
Write a predicate UPSTAIRSP that returns T if its input is an up‐
stairs location. (The library and the upstairs bedroom are the only
two locations upstairs.) Write a predicate ONSTAIRSP that returns
T if its input is either FRONT-STAIRS or BACK-STAIRS.
|#

(defun upstairsp (loc)
  (if (or (eql loc 'library)
          (eql loc 'upstairs-bedroom))
      t))

(defun onstairsp (loc)
  (if (or (eql loc 'front-stairs)
          (eql loc 'back-stairs))
      t))

#|
Where’s Robbie? Write a function of no inputs called WHERE
that tells where Robbie is. If he is in the library, (WHERE)
should say (ROBBIE IS UPSTAIRS IN THE LIBRARY). If he is in the
kitchen, it should say (ROBBIE IS DOWNSTAIRS IN THE KITCHEN).
If he is on the front stairs, it should say
(ROBBIE IS ON THE FRONT-STAIRS).
|#

(defun where ()
  (cond ((onstairsp loc)
         (format t "Robbie is on the ~a" loc))
        ((upstairsp loc)
         (format t "Robbie is upstairs in the ~a" loc))
        (t (format t "Robie is donwstairs in the ~a" loc))))

#|
Write a function MOVE that takes one input, a direction, and
moves Robbie in that direction. MOVE should make use of the
LOOK function you wrote previously, and should call
SET-ROBBIE- LOCATION to move him. If Robbie can’t move in the speci‐
fied direction an appropriate message should be returned. For ex‐
ample, if Robbie is in the pantry, (MOVE ‘SOUTH) should return
something like (OUCH! ROBBIE HIT A WALL). (MOVE
‘NORTH) should change Robbie’s location and return (ROBBIE IS
DOWNSTAIRS IN THE KITCHEN).
|#

(defun move (dir)
  (let ((path (look dir loc)))
    (if path
        (progn (set-robbie-location path) (where))
        (format t "Ouch, Robbie hit a wall!"))))
