(defparameter rooms 
  '((library (east upstairs-bedroom) (south back-stairs))
    (back-stairs (north library) (south downstairs-bedroom))
    (downstairs-bedroom (north back-stairs) (east dining-room))
    (upstairs-bedroom (west library) (south front-stairs))
    (front-stairs (north upstairs-bedroom) (south living-room))
    (living-room (north front-stairs) (east kitchen) (south dining-room))
    (dining-room (north living-room) (west downstairs-bedroom) (east pantry))
    (kitchen (west living-room) (south pantry))
    (pantry (north kitchen) (west dining-room))))

(defun choices (room)
  (rest (assoc room rooms)))

(defun look (dir room)
  (cadr (assoc dir (choices room))))

(defvar loc 'pantry)

(defun set-robbie-location (place)
  (setf loc place))

(defun how-many-choices ()
  (length (choices loc)))

(defun upstairsp (loc)
  (if (or (eql loc 'library)
          (eql loc 'upstairs-bedroom))
      t))

(defun onstairsp (loc)
  (if (or (eql loc 'front-stairs)
          (eql loc 'back-stairs))
      t))

(defun where ()
  (cond ((onstairsp loc)
         (format t "ROBBIE IS ON THE ~A" loc))
        ((upstairsp loc)
         (format t "ROBBIE IS UPSTAIRS IN THE ~A" loc))
        (t (format t "ROBBIE IS DOWNSTAIRS IN THE ~A" loc))))

(defun move (dir)
  (let ((path (look dir loc)))
    (if path
    (progn (set-robbie-location path) (where))
    (format t "OUCH, ROBBIE HIT A WALL"))))
