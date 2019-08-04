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
  (let ((result nil)
	(moves (sort (select-moves-with-color color move-list)
		     #'<=
		     :key (lambda (x) (first (first x))))))
    (dolist (move moves)
      (if (or
	   (check-horizontal-win move moves)
	   (check-vertical-win move moves)
	   (check-diagonal-win move moves))
	  (setf result t)))
    result))

(defun in-row-iter (move move-list dx dy n)
  (let* ((next (get-piece-at-space (+ dx (move-x move))
				   (+ dy (move-y move))
				   (cdr move-list))))
    (cond
      ((= n 4) t)
      ((null next) nil)
      (t (in-row-iter next (remove next (remove move move-list)) dx dy (1+ n)))))) 

(defun check-horizontal-win (move move-list)
  (in-row-iter move move-list 1 0 1))

(defun check-vertical-win (move move-list)
  (in-row-iter move move-list 0 1 1))

(defun check-diagonal-win (move move-list)
  (in-row-iter move move-list 1 1 1))

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
