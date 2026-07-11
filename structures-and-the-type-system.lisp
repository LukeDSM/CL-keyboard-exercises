#|
Write a DEFSTRUCT for a structure called NODE, with four com‐
ponents called NAME, QUESTION, YES-CASE, and NO- CASE.
|#

(defstruct node
  name
  question
  yes-case
  no-case)

#|
Define a global variable *NODE-LIST* that will hold all the nodes
in the discrimination net. Write a function INIT that initializes the
network by setting *NODE-LIST* to NIL.
|#

(defvar *node-list* ())

(defun init ()
  (setf *node-list* nil))

#|
Write ADD-NODE. It should return the name of the node it added.
|#

(defun add-node (n q yc nc)
  (node-name
    (first 
      (push (make-node :name n
                      :question q
                      :yes-case yc
                      :no-case nc)
            *node-list*))))

#|
Write FIND-NODE, which takes a node name as input and returns
the node if it appears in *NODE-LIST*, or NIL if it doesn’t.
|#

(defun find-node (name)
  (first (remove-if-not (lambda (e)
			  (string= (node-name e) name))
			*node-list*)))

; Lisp code to create the automotive diagnosis network.

(add-node 'start
	  "Does the engine turn over?"
	  'engine-turns-over
	  'engine-wont-turn-over)

(add-node 'engine-turns-over
	  "Will the engine run for any period of time?"
	  'engine-will-run-briefly
	  'engine-wont-run)

(add-node 'engine-wont-run
	  "Is there gas in the tank?"
	  'gas-in-tank
	  "Fill the tank and try starting the engine again.")

(add-node 'engine-wont-turn-over
	  "Do you hear any sound when you turn the key?"
	  'sound-when-turn-key
	  'no-sound-when-turn-key)

(add-node 'no-sound-when-turn-key
	  "Is the battery voltage low?"
	  "Replace the battery"
	  'battery-voltage-ok)

(add-node 'battery-voltage-ok
	  "Are the battery cables dirty or loose?"
	  "Clean the cables and tighten the connections."
	  'battery-cables-good)

#|
Write PROCESS-NODE. It takes a node name as input. If it can’t
find the node, it prints a message that the node hasn’t been defined
yet, and returns NIL. Otherwise it asks the user the question associ‐
ated with that node, and then returns the node’s yes action or no ac‐
tion depending on how the user responds.
|#

(defun process-node (name)
  (let ((node (find-node name)))
    (unless node
      (format t "~&Node undefined.~%")
      (return-from process-node))
    (if (yes-or-no-p (node-question node))
	(node-yes-case node)
	(node-no-case node))))

#|
Write the function RUN. It maintains a local variable named CUR‐
RENT-NODE, whose initial value is START. It loops, calling
PROCESS-NODE to process the current node, and storing the
value returned by PROCESS-NODE back into CURRENT-NODE.
If the value returned is a string, the function prints the string and
stops. If the value returned is NIL, it also stops.
|#

(defun run ()
  (let ((current-node 'start))
    (loop
      (let ((result (process-node current-node)))
        (cond ((stringp result) (return (format t "~a~%" result)))
              ((null result) (return))
              (t (setf current-node result)))))))

#|
Write an interactive function to add a new node. It should prompt
the user for the node name, the question, and the yes and no ac‐
tions. Remember that the question must be a string, enclosed in
double quotes. Your function should add the new node to the net.
|#

(defun interactive-add-node ()
  (let (n q yc nc)
    (format t "~&Node name?~%")
    (finish-output)
    (setf n (read))
    (format t "Node question?~%")
    (finish-output)
    (setf q (read-line))
    (format t "Node yes case?~%")
    (finish-output)
    (setf yc (read))
    (format t "Node no case?~%")
    (finish-output)
    (setf nc (read))
    
    (add-node n q yc nc)))
