;;;; Advent of Code 2021, Day 2, Part1
;;;; https://adventofcode.com/2021/day/2
;;;; Solution by Darren Stone
;;;; <dstone@bitmason.com>

(defun day-02-part-1 (input)
  (let* ((hor-deps (mapcar 'cmd-to-hor-dep input))
         (hors (mapcar 'car hor-deps))
         (deps (mapcar 'second hor-deps))
         (final-pos (list (reduce '+ hors) (reduce '+ deps))))
    (reduce '* final-pos)))

;;; given cmd, return delta for (hor dep)
;;; e.g. "forward 5" => (5 0), "up 2" => (0 -2)
(defun cmd-to-hor-dep (cmd)
  (match (words cmd)
    ((list "forward" n) (list (parse-integer n) 0))
    ((list "up" n) (list 0 (- (parse-integer n))))
    ((list "down" n) (list 0 (parse-integer n)))))
