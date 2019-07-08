(defparameter *max-captures* 10)

(defun make-board ()
  ; TODO
  )

(defun place-piece (x y color)
  ; TODO
  )

(defun five-in-row (board color)
  ; TODO
  )

(defun captured-enough-for-win (num-captured)
  (if (>= num-captured *max-captures*)
      t
      nil))
