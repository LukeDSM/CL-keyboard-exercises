#|
Here is an example of the function MYFUN, a strange function of
two inputs.
(myfun 'alpha 'beta) -> ((ALPHA) BETA)
Write MYFUN. Test your function to make certain it works correctly.
|#

(defun myfun (x y)
  (list (list x) y))

#|
Write a predicate FIRSTP that returns T if its first argument (a
symbol) is equal to the first element of its second argument (a list).
That is, (FIRSTP ‘FOO ‘(FOO BAR BAZ)) should return T.
(FIRSTP ‘BOING ‘(FOO BAR BAZ)) should return NIL.
|#

(defun firstp (s lst)
  (eql s (first lst)))

#|
Write a function MID-ADD1 that adds 1 to the middle element of a
three-element list. For example, (MID-ADD 1 ‘(TAKE 2 COOKIES))
should return the list (TAKE 3 COOKIES). Note: You are
not allowed to make MID-ADD 1 a function of three inputs. It has
to take a single input that is a list of three elements.
|#

(defun midd-add1 (lst)
  (list (first lst)
        (1+ (second lst))
        (third lst)))

#|
Write a function F-TO-C that converts a temperature from Fahren‐
heit to Celsius. The formula for doing the conversion is: Celsius
temperature = [5 x (Fahrenheit temperature - 32)]/9. To go in the
opposite direction, the formula is: Fahrenheit temperature = (9/5 x
Celsius temperature) + 32
|#

(defun f-to-c (temp)
  (/ (* 5 (- temp 32)) 9))
