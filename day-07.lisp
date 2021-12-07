;;;; Advent of Code 2021, Day 7
;;;; https://adventofcode.com/2021/day/7
;;;; Solution by Darren Stone <dstone@bitmason.com>

(defun day-07 (input)
  (let ((crabs (mapcar #'parse-integer (split "," (car input)))))
    (mapcar (lambda (fuel-calc) (min-fuel fuel-calc crabs))
            (list 'fuel-calc-part-1 
                  'fuel-calc-part-2))))

(defun min-fuel (fuel-calc crabs) ; minimum fuel to an aligned destination
  (reduce #'min
          (mapcar (lambda (dest) (apply fuel-calc (list dest crabs)))
                  (loop for dest
                        from (reduce #'min crabs)
                          to (reduce #'max crabs)
                        collect dest))))

(defun fuel-calc-part-1 (dest crabs) ; total fuel to move all crabs to dest (part 1)
  (reduce #'+
          (mapcar (lambda (crab) (abs (- crab dest)))
                  crabs)))

(defun fuel-calc-part-2 (dest crabs) ; total fuel to move all crabs to dest (part 2)
  (reduce #'+
          (mapcar (lambda (crab)
                    (loop for n to (abs (- crab dest))
                          sum n))
                  crabs)))
