(defconstant MAX-CAPTURES 10)
(defconstant WHITE 'W)
(defconstant BLACK 'B)
(defconstant EMPTY 'E)
(defconstant BOARD-SIZE '(13 13))

(defstruct move
  x
  y
  color)

(defun move-at (x y)
  (lambda (move)
    (equal (list (move-x move) (move-y move))
	   (list x y))))

(defstruct gamestate
  move-list
  turn
  nwhite-captures
  nblack-captures)

(defun make-board-from-move-list (move-list)
  (let ((board (make-array BOARD-SIZE :initial-element EMPTY)))
    (dolist (move move-list)
      (setf (aref board (move-y move) (move-x move)) (move-color move)))
    board))

(defun get-neighbors-of (x y move-list)
  (if (null (get-piece-at-space x y move-list))
      nil
      (remove-if #'null
		 (list (get-piece-at-space (1- x) y move-list)
		       (get-piece-at-space (1- x) (1- y) move-list)
		       (get-piece-at-space x (1- y) move-list)
		       (get-piece-at-space (1+ x) (1+ y) move-list)
		       (get-piece-at-space (1+ x) y move-list)
		       (get-piece-at-space (1+ x) (1+ y) move-list)
		       (get-piece-at-space x (1+ y) move-list)
		       (get-piece-at-space (1- x) (1+ y) move-list)))))

(defun remove-piece (x y move-list)
  (remove-if (move-at x y) move-list))

(defun place-piece (x y color move-list)
  (cons (make-move :x x :y y :color color) move-list))

(defun get-piece-at-space (x y move-list)
  (find-if (move-at x y) move-list))

(defun is-five-in-row (color move-list)
  (let ((result nil)
	(moves (select-moves-with-color color move-list)))
    (dolist (move moves)
      (if (or
	   (check-horizontal-win move moves)
	   (check-vertical-win move moves)
	   (check-diagonal-win move moves))
	  (setf result t)))
    result))
    
(defun in-row-iter (move move-list dx dy n goal)
  "Returns true if there are 'goal' number of pieces in a direction in move-list"
  (let ((next (get-piece-at-space (+ dx (move-x move))
				   (+ dy (move-y move))
				   (cdr move-list))))
    (cond
      ((= n goal) t)
      ((null next) nil)
      (t (in-row-iter next
		      (remove next (remove move move-list))
		      dx dy (1+ n) goal))))) 

(defun check-horizontal-win (move move-list)
  (in-row-iter move move-list 1 0 1 4))

(defun check-vertical-win (move move-list)
  (in-row-iter move move-list 0 1 1 4))

(defun check-diagonal-win (move move-list)
  (in-row-iter move move-list 1 1 1 4))

(defun get-captures (move-list)
  (let ((captures nil))
    (dolist (move move-list)
      (setf captures
	    (append
	     ;; we only search left to right/top to bottom so
	     ;; we don't get duplicates
	     (append (is-capture-iter move move-list 1 0)
		     (is-capture-iter move move-list 1 1)
		     (is-capture-iter move move-list 0 1)
		     (is-capture-iter move move-list -1 1))
	     captures)))
    captures));;TODO: Remove duplicate capture locations

(defun is-capture-iter (move move-list dx dy)
  (let* ((color (move-color move))
	 (friendly (select-moves-with-color color move-list))
	 (enemy (if (eq color WHITE)
		    (select-moves-with-color BLACK move-list)
		    (select-moves-with-color WHITE move-list))))
    (if (and (in-row-iter move enemy dx dy 1 2)
	     (get-piece-at-space (if (eq dx 1) (+ (move-x move) 3) (move-x move))
				 (if (eq dy 1) (+ (move-y move) 3) (move-y move))
				 friendly))
	(list (list (+ (move-x move) dx) (+ (move-y move) dy))
	      (list (+ (move-x move) (* dx 2)) (+ (move-y move) (* dy 2))))
	nil)))
      
(defun select-moves-with-color (color move-list)
    (remove-if (lambda (move)
                 (if (eq (move-color move) color)
                     nil
                     t))
               move-list))

(defun captured-enough-for-win (num-captured)
  (if (>= num-captured MAX-CAPTURES)
      t
      nil))
