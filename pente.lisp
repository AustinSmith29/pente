(defparameter *max-captures* 10)
(defparameter *white* 'W)
(defparameter *black* 'B)
(defparameter *empty* 'E)
(defparameter *board-size* '(13 13))

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

(defun remove-piece (x y move-list)
  (remove-if (lambda (move)
               (equal (move-coord move) (list x y)))
             move-list))

(defun place-piece (x y color move-list)
  (cons (cons (list x y) color) move-list))

(defun piece-at-space (x y move-list)
  (find-if (lambda (move)
             (equal (move-coord move) (list x y)))
           move-list))

(defun is-five-in-row (color move-list)
  (let ((moves (filter-color-out color move-list)))))

(defun filter-color-out (color move-list)
    (remove-if (lambda (move)
                 (if (eq (move-color move) color)
                     t
                     nil))
               move-list))

(defun captured-enough-for-win (num-captured)
  (if (>= num-captured *max-captures*)
      t
      nil))
