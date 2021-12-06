;;;; Advent of Code 2021, Day 6
;;;; https://adventofcode.com/2021/day/6
;;;; Solution by Darren Stone <dstone@bitmason.com>

(defun day-06 (input)
  (list (length (fish-days 80 (mapcar #'parse-integer (split "," (car input))))) ; part 1
        (length (fish-days 256 (mapcar #'parse-integer (split "," (car input))))))) ; part 2

(defun fish-days (days fish) ; return fish state after given # days
  (if (= days 0)
      fish
      (fish-days (- days 1)
                 (append (mapcar (lambda (d)
                                   (if (= d 0)
                                       6
                                       (- d 1)))
                                 fish)
                         (make-list (count 0 fish)
                                    :initial-element 8)))))
