;;;; Advent of Code 2021, Day 6
;;;; https://adventofcode.com/2021/day/6
;;;; Solution by Darren Stone <dstone@bitmason.com>

(defun day-06 (input)
  (list (reduce #'+ (fish-days 80 (fish-counts input)))
        (reduce #'+ (fish-days 256 (fish-counts input)))))

(defun fish-counts (input) ; csv to fish count for day values (0,1,2,...,8)
  (loop for d upto 8
        collect (count d (mapcar #'parse-integer (split "," (car input))))))

(defun fish-days (days fish) ; return fish counts after given # days
  (if (= days 0)
      fish
      (fish-days (- days 1)
                 (append (subseq fish 1 7)
                         (list (+ (nth 0 fish) (nth 7 fish)))
                         (list (nth 8 fish) (nth 0 fish))))))
