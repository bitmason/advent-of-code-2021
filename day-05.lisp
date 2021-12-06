;;;; Advent of Code 2021, Day 5
;;;; https://adventofcode.com/2021/day/5
;;;; Solution by Darren Stone <dstone@bitmason.com>

(defun day-05 (input)
  (let ((lines (mapcar #'parse-vent-line input)))
    (list (overlap-count (remove-if-not #'line-orthag-p lines)) ; part 1
          (overlap-count lines)))) ; part 2

(defun parse-vent-line (line)
  (ppcre:register-groups-bind ((#'parse-integer x1 y1 x2 y2))
      ("(\\d+),(\\d+) -> (\\d+),(\\d+)" line) (list x1 y1 x2 y2)))

(defun overlap-count (lines)
  (let ((m (make-array '(1000 1000)))) ; empirically good enough!
    (loop for line in lines
          do (loop for p in (line-points line)
                   do (setf (apply #'aref m p) (+ 1 (apply #'aref m p)))))
    (loop for y below 1000 sum (loop for x below 1000 if (> (aref m x y) 1) sum 1))))

(defun line-orthag-p (coords) (destructuring-bind (x1 y1 x2 y2) coords (or (= x1 x2) (= y1 y2))))

(defun range (a b) ; inclusive (may be asc or desc)
    (if (< a b) (loop for n from a to b collect n) (loop for n from a downto b collect n)))

(defun line-points (line) ; list of (x y) covered by line (x1 y1 x2 y2)
  (destructuring-bind (x1 y1 x2 y2) line
    (if (= x1 x2) (loop for y in (range y1 y2) collect (list x1 y))
        (if (= y1 y2) (loop for x in (range x1 x2) collect (list x y1))
            (loop for x in (range x1 x2)
                  for y in (range y1 y2)
                  collect (list x y))))))
