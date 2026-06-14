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
