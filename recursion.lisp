(defparameter family
  '((colin nil nil)
    (deirdre nil nil)
    (arthur nil nil)
    (kate nil nil)
    (frank nil nil)
    (linda nil nil)
    (suzanne colin deirdre)
    (bruce arthur kate)
    (charles arthur kate)
    (david arthur kate)
    (ellen arthur kate)
    (george frank linda)
    (hillary frank linda)
    (andre nil nil)
    (tamara bruce suzanne)
    (vincent bruce suzanne)
    (wanda nil nil)
    (ivan george ellen)
    (julie george ellen)
    (marie george ellen)
    (nigel andre hillary)
    (frederick nil tamara)
    (zelda vincent wanda)
    (joshua ivan wanda)
    (quentin nil nil)
    (robert quentin julie)
    (olivia nigel marie)
    (peter nigel marie)
    (erica nil nil)
    (yvette robert zelda)
    (diane peter erica)))
    ;child father mother

#|
Write the functions FATHER, MOTHER, PARENTS, and CHILDREN
that return a person’s father, mother, a list of his or her
known parents, and a list of his or her children, respectively.
(FATHER ’SUZANNE) should return COLIN. (PARENTS ’SUZANNE)
should return (COLIN DEIRDRE). (PARENTS ’FREDERICK)
should return (TAMARA), since Frederick’s father is unknown.
(CHILDREN ’ARTHUR) should return the set (BRUCE CHARLES DAVID ELLEN).
If any of these functions is given NIL as input, it should return NIL.
This feature will be useful later when we write some recursive functions.
|#

(defun father (name)
  (second (assoc name family)))

(defun mother (name)
  (third (assoc name family)))

(defun parents (name)
  (remove nil
    (rest (assoc name family))))

(defun children (name)
  (unless (null name)
    (mapcar #'first
      (remove-if-not (lambda (fam)
                      (member name (rest fam)))
                     family))))

#|
Write SIBLINGS, a function that returns a list of a person’s siblings,
including genetic half-siblings. (SIBLINGS ’BRUCE) should return
(CHARLES DAVID ELLEN). (SIBLINGS ’ZELDA) should return (JOSHUA).
|#

(defun siblings (name)
  (remove name
    (union (children (father name))
           (children (mother name)))))

#|
Write MAPUNION, an applicative operator that takes a function
and a list as input, applies the function to every element of the list,
and computes the union of all the results. An example is
(MAPUNION #’REST ’((1 A B C) (2 E C J) (3 F A B C D))), which
should return the set (A B C E J F D). Hint: MAPUNION can be
defined as a combination of two applicative operators you already
know.
|#

(defun mapunion (func lst)
  (remove-duplicates
    (reduce #'append
      (mapcar func lst))
    :from-end t))

#|
Write GRANDPARENTS, a function that returns the set of
a person’s grandparents. Use MAPUNION in your solution.
|#

(defun grandparents (name)
  (mapunion #'parents (parents name)))

#|
Write COUSINS, a function that returns the set of a person’s genet‐
ically related first cousins, in other words, the children of any of
their parents’ siblings. (COUSINS ’JULIE) should return the set
(TAMARA VINCENT NIGEL). Use MAPUNION in your solution.
|#

(defun cousins (name)
  (reverse ; ?? XD
    (mapunion #'children
      (mapunion #'siblings (parents name)))))

#|
Write the two-input recursive predicate DESCENDED-FROM that
returns a true value if the first person is descended from the second.
(DESCENDED-FROM ’TAMARA ’ARTHUR) should return T.
(DESCENDED-FROM ’TAMARA ’LINDA) should return NIL.
(Hint: You are descended from someone if he is one of your parents,
or if either your father or mother is descended from him. This
is a recursive definition.)
|#

(defun descended-from (name ancestor)
  (cond ((or (null (parents name)) (null name)) nil)
        ((member ancestor (parents name)) t)
        ((or (descended-from (father name) ancestor)
             (descended-from (mother name) ancestor))
         t)))

#|
Write the recursive function ANCESTORS that returns a person’s
set of ancestors. (ANCESTORS ’MARIE) should return the set
(ELLEN ARTHUR KATE GEORGE FRANK LINDA). (Hint: A person’s ancestors
are his parents plus his parents’ ancestors. This is a recursive definition.)
|#

(defun ancestors (name)
  (cond ((null name) nil)
        (t (union (parents name)
                  (union (ancestors (father name))
                         (ancestors (mother name)))))))

#|
Write the recursive function GENERATION-GAP that returns the
number of generations separating a person and one of his or her 
ancestors. (GENERATION-GAP ’SUZANNE ’COLIN) should return
one. (GENERATION-GAP ’FREDERICK’COLIN) should return
three. (GENERATION-GAP ’FREDERICK ’LINDA) should return
NIL, because Linda is not an ancestor of Frederick.
|#

(defun generation-gap (name ancestor)
  (cond ((or (null (parents name)) (null name)) nil)
        ((member ancestor (parents name)) 1)
        ((generation-gap (father name) ancestor)
         (1+ (generation-gap (father name) ancestor)))
        ((generation-gap (mother name) ancestor)
         (1+ (generation-gap (mother name) ancestor)))))