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
  (let ((moves (sort (select-moves-with-color color move-list)
		     #'<=
		     :key #'first)))
    (is-five-in-row-iter moves)))

(defun is-five-in-row-iter (move-list)
  (let ((move (car move-list)))
    (cond
      ((check-horizontal-win move (cdr move-list) 1) t)
      ((check-vertical-win move (cdr move-list) 1) t)
      ((check-diagonal-win move (cdr move-list) 1) t)
      (t nil))))

(defun check-horizontal-win (move move-list n)
  (cond
    ((null move-list) nil)
    ((= n 5) t)
    ((null (get-piece-at-space (+ 1 (move-x move))
				    (move-y move)
				    (cdr move-list)))
     nil)
    (t (let ((newmove (get-piece-at-space (+ 1 (move-x move))
					  (move-y move)
					  (cdr move-list))))
	 (check-horizontal-win newmove (cdr move-list) (+ 1 n))))))

(defun check-vertical-win (move move-list n)
  (cond
    ((null move-list) nil)
    ((= n 5) t)
    ((null (get-piece-at-space (move-x move)
			       (+ 1 (move-y move))
			       (cdr move-list)))
     nil)
    (t (let ((newmove (get-piece-at-space (move-x move)
					  (+ 1 (move-y move))
					  (cdr move-list))))
	 (check-vertical-win newmove (cdr move-list) (+ 1 n))))))

(defun check-diagonal-win (move move-list n)
  (cond
    ((null move-list) nil)
    ((= n 5) t)
    ((null (get-piece-at-space (+ 1 (move-x move))
			       (+ 1 (move-y move))
			       (cdr move-list)))
     nil)
    (t (let ((newmove (get-piece-at-space (+ 1 (move-x move))
					  (+ 1 (move-y move))
					  (cdr move-list))))
	 (check-diagonal-win newmove (cdr move-list) (+ 1 n))))))

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
