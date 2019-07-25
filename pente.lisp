(defconstant *max-captures* 10)
(defconstant *white* 'W)
(defconstant *black* 'B)
(defconstant *empty* 'E)
(defconstant *board-size* '(13 13))

(defun move-x (move)
  (car (car move)))

(defun move-y (move)
  (car (cdr (car move))))

(defun move-coord (move)
  (car move))

(defun move-color (move)
  (cdr move))

(defun make-board-from-move-list (move-list)
  (let ((board (make-array *board-size* :initial-element *empty*)))
    (dolist (move move-list)
      (setf (aref board (move-x move) (move-y move)) (move-color move)))
    board))

(defun move-at (move x y)
  (equal (move-coord move) (list x y)))

(defun remove-piece (x y move-list)
  (remove-if (lambda (move)
               (equal (move-coord move) (list x y)))
             move-list))

(defun place-piece (x y color move-list)
  (cons (cons (list x y) color) move-list))

(defun get-piece-at-space (x y move-list)
  (find-if (lambda (move)
             (equal (move-coord move) (list x y)))
           move-list))

(defun is-five-in-row (color move-list)
  (let ((moves (select-moves-with-color color move-list)))
    (is-five-in-row-iter color moves 1)))

(defun is-five-in-row-iter (color move-list direction count)
  (let ((move (car move-list)))
    ()))

(defun select-moves-with-color (color move-list)
    (remove-if (lambda (move)
                 (if (eq (move-color move) color)
                     nil
                     t))
               move-list))

(defun captured-enough-for-win (num-captured)
  (if (>= num-captured *max-captures*)
      t
      nil))
