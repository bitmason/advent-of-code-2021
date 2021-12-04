;;;; Advent of Code 2021, Day 2
;;;; https://adventofcode.com/2021/day/2
;;;; Solution by Darren Stone
;;;; <dstone@bitmason.com>

(defun day-02-part-1 (input)
  (let* ((hor-deps (mapcar 'cmd-to-hor-dep input))
         (hors (mapcar 'car hor-deps))
         (deps (mapcar 'second hor-deps))
         (final-pos (list (reduce '+ hors) (reduce '+ deps))))
    (reduce '* final-pos)))

(defun day-02-part-2 (input)
  (reduce '* (butlast (reduce 'state-cmd-apply input
                              :initial-value '(0 0 0)))))

;;; given cmd, return delta for (hor dep)
;;; e.g. "forward 5" => (5 0), "up 2" => (0 -2)
(defun cmd-to-hor-dep (cmd)
  (match (words cmd)
    ((list "forward" n) (list (parse-integer n) 0))
    ((list "up" n) (list 0 (- (parse-integer n))))
    ((list "down" n) (list 0 (parse-integer n)))))

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
