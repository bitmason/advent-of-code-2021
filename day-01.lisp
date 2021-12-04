;;;; Advent of Code 2021, Day 1
;;;; https://adventofcode.com/2021/day/1
;;;; Solution by Darren Stone <dstone@bitmason.com>

(let* ((depths (mapcar 'parse-integer input))

       (depth-diffs (mapcar '- (cdr depths) depths))

       (window-sums (mapcar '+ depths (cdr depths) (cddr depths)))
       (window-sum-diffs (mapcar '- (cdr window-sums) window-sums)))

  (list (length (remove-if-not 'plusp depth-diffs))        ; part 1
        (length (remove-if-not 'plusp window-sum-diffs)))) ; part 2
