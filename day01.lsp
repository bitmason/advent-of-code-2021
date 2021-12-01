#!/usr/bin/env sbcl --script
(load "~/quicklisp/setup.lisp")

; Advent of Code 2021, Day 1
; https://adventofcode.com/2021/day/1
; Solution by Darren Stone <dstone@bitmason.com>

(let* ((depths (mapcar 'parse-integer (uiop:read-file-lines "day01_input.txt")))

       (depth-diffs (mapcar '- (cdr depths) depths))

       (window-sums (mapcar '+ depths (cdr depths) (cddr depths)))
       (window-sum-diffs (mapcar '- (cdr window-sums) window-sums)))

  (format t "Part One: ~A~%" (length (remove-if-not 'plusp depth-diffs)))
  (format t "Part Two: ~A~%" (length (remove-if-not 'plusp window-sum-diffs))))
