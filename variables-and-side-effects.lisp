#|
Write a function THROW-DIE that returns a random number from
1 to 6, inclusive. Remember that (RANDOM 6) will pick numbers
from 0 to 5. THROW-DIE doesn’t need any inputs, so its argument
list should be NIL.
|#

(defun throw-die ()
  (1+ (random 6)))

#|
Write a function THROW-DICE that throws two dice and returns a
list of two numbers: the value of the first die and the value of the
second. We’ll call this list a “throw.” For example, (THROW-
DICE) might return the throw (3 5), indicating that the first die was
a 3 and the second a 5.
|#

(defun throw-dice ()
  (list (throw-die)
	(throw-die)))

#|
Throwing two ones is called “snake eyes”; two sixes is called “box-
cars.” Write predicates SNAKE-EYES-P and BOXCARS-P that
take a throw as input and return T if the throw is equal to (1 1) or (6
6), respectively.
|#

(defun snake-eyes-p (throw)
  (equal throw '(1 1)))

(defun boxcars-p (throw)
  (equal throw '(6 6)))

#|
In playing craps, the first throw of the dice is crucial. A throw of 7
or 11 is an instant win. A throw of 2, 3, or 12 is an instant loss
(American casino rules). Write predicates INSTANT-WIN-P and
INSTANT-LOSS-P to detect these conditions. Each should take a
throw as input.
|#

(defun instant-win-p (throw)
  (let ((point (+ (first throw)
		  (second throw))))
    (or (= point 7)
        (= point 11))))

(defun instant-loss-p (throw)
  (let ((point (+ (first throw)
		  (second throw))))
  (or (= point 2)
      (= point 3)
      (= point 12))))

#|
Write a function SAY-THROW that takes a throw as input and re-
turns either the sum of the two dice or the symbol SNAKE-EYES
or BOXCARS if the sum is 2 or 12. (SAY-THROW ‘(3 4)) should
return 7. (SAY-THROW ‘(6 6)) should return BOXCARS.
|#

(defun say-throw (throw)
  (cond ((snake-eyes-p throw) 'snake-eyes)
	((boxcars-p throw) 'boxcars)
	(t (+ (first throw)
	      (second throw)))))

#|
If you don’t win or lose on the first throw of the dice, the value
you threw becomes your “point,” which will be explained shortly.
Write a function (CRAPS) that produces the following sort of behavior.
Your solution should make use of the functions you wrote in previous steps.

> (craps)
(THROW 1 AND 1 -- SNAKEYES -- YOU LOSE)

> (craps)
(THROW 3 AND 4 -- 7 -- YOU WIN)

> (craps)
(THROW 2 AND 4 -- YOUR POINT IS 6)
|#

(defun craps ()
  (let* ((throw (throw-dice))
	 (point (say-throw throw)))
    (if (or (instant-win-p throw)
	    (instant-loss-p throw))
	`(throw ,(first throw) and ,(second throw) -- ,point -- you ,(if (instant-win-p throw) 'win 'lose))
	`(throw ,(first throw) and ,(second throw) -- your point is ,(say-throw throw)))))

#|
Once a point has been established, you continue throwing the dice
until you either win by making the point again or lose by throwing
a 7. Write the function TRY-FOR-POINT that simulates this part of
the game, as follows:

> (try-for-point 6)
(THROW 3 AND 5 -- 8 -- THROW AGAIN)

> (try-for-point 6)
(THROW 5 AND 1 -- 6 -- YOU WIN)

> (craps)
(THROW 3 AND 6 -- YOUR POINT IS 9)

> (try-for-point 9)
(THROW 6 AND 1 -- 7 -- YOU LOSE)
|#

(defun try-for-point (n)
  (let* ((throw (throw-dice))
	 (point (+ (first throw)
		   (second throw))))
    `(throw ,(first throw) and ,(second throw) -- ,point -- ,@(cond ((= point n) '(you win))
								    ((= point 7) '(you lose))
							            (t '(throw again))))))
