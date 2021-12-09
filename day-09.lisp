;;;; Advent of Code 2021, Day 9
;;;; https://adventofcode.com/2021/day/9
;;;; Solution by Darren Stone <dstone@bitmason.com>

(defun day-09 (input)
  (let* ((y-size (length input))
         (x-size (length (car input)))
         (heightmap (make-array (list y-size x-size))))
    (loop for line in input
          for y below y-size
          do (loop for h in (coerce line 'list)
                   for x below x-size
                   do (setf (aref heightmap y x)
                            (parse-integer (string h)))))
    ;; just part 1 for now
    (loop for y below y-size
          sum (loop for x below x-size
                    for h = (aref heightmap y x)
                    when (< h (reduce #'min (adj-heights heightmap y x)))
                      sum (+ h 1)))))

(defun adj-heights (heightmap y x)  ; list of 2-4 neighbour heights (NESW)
  (loop for off in '((-1 0) (0 1) (1 0) (0 -1))
        for ny = (+ y (car off))
        for nx = (+ x (second off))
        when (and (>= ny 0)
                  (< ny (array-dimension heightmap 0))
                  (>= nx 0)
                  (< nx (array-dimension heightmap 1)))
          collect (aref heightmap ny nx)))
