;;;; Advent of Code 2021, Day 3, Part 1
;;;; https://adventofcode.com/2021/day/3
;;;; Solution by Darren Stone
;;;; <dstone@bitmason.com>

(defun day-03-part-1 (input)
  (let* ((lines-by-bit-at-pos (loop for n below (length (car input)) ; pair of lists per bit pos
                                    collect (lines-by-nth-bit n input)))
         (gamma-rate (chars-to-int (mapcar (lambda (nth) (if (> (length (car nth))
                                                                (length (cadr nth)))
                                                             #\0 #\1)) lines-by-bit-at-pos)))
         (epsilon-rate (logxor (- (expt 2 (length (car input))) 1) gamma-rate)))
    (* gamma-rate epsilon-rate)))

(defun nth-bit-set-p (n str) (eq #\1 (nth n (coerce str 'list)))) ; nth char is #\1 ?

(defun lines-by-nth-bit (n lines) ; split to pair of lists: (lines-with-nth-bit-0 lines-with-nth-bit-1)
  (list (remove-if (lambda (str) (nth-bit-set-p n str)) lines)
        (remove-if (lambda (str) (not (nth-bit-set-p n str))) lines)))

(defun chars-to-int (chars) (parse-integer (coerce chars 'string) :radix 2)) ; e.g. (#\1 #\0 #\1) => 5
