;;;; Advent of Code 2021, Day 2, Part 2
;;;; https://adventofcode.com/2021/day/2
;;;; Solution by Darren Stone
;;;; <dstone@bitmason.com>

;;; given state (hor dep aim), apply cmd & return new state
;;; e.g. (1 2 3) "forward 5" => (6 17 3), (1 2 3) "up 2" => (1 2 1)
(defun state-cmd-apply (state cmd)
  (let ((dir (car (words cmd)))
        (n (parse-integer (second (words cmd)))))
    (destructuring-bind (hor dep aim) state
      (match dir
        ("forward" (list (+ hor n) (+ dep (* aim n)) aim))
        ("up" (list hor dep (- aim n)))
        ("down" (list hor dep (+ aim n)))))))

(defun day-02-part-2 (input)
  (reduce '* (butlast (reduce 'state-cmd-apply input
                              :initial-value '(0 0 0)))))
