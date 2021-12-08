;;;; Advent of Code 2021, Day 8
;;;; https://adventofcode.com/2021/day/8
;;;; Solution by Darren Stone <dstone@bitmason.com>

(defun day-08 (input)
  (loop for line in input
        for entry = (mapcar #' words (split "|" line))
        for signal-patterns = (first entry) ; e.g. ("cg" "fgaced" ...)
        for output-digits = (second entry) ; e.g. ("bcgaf" "edf" ...)
        sum (length (remove-if-not ; unique seg count for digits 1 4 7 8
                     (lambda (segs) (member (length segs) '(2 4 3 7)))
                     output-digits))))
