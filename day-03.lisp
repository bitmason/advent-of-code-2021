;;;; Advent of Code 2021, Day 3
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

(defun day-03-part-2 (input)
  (* (parse-integer (car (reduce #'o2-nums (loop for pos below (length (car input))
                                                 collect pos) :initial-value input)) :radix 2)
     (parse-integer (car (reduce #'co2-nums (loop for pos below (length (car input))
                                                  collect pos) :initial-value input)) :radix 2)))

(defun nth-bit-set-p (n str) (eq #\1 (nth n (coerce str 'list)))) ; nth char is #\1 ?

(defun lines-by-nth-bit (n lines) ; split to pair of lists: (lines-with-nth-bit-0 lines-with-nth-bit-1)
  (list (remove-if (lambda (str) (nth-bit-set-p n str)) lines)
        (remove-if (lambda (str) (not (nth-bit-set-p n str))) lines)))

(defun chars-to-int (chars) (parse-integer (coerce chars 'string) :radix 2)) ; e.g. (#\1 #\0 #\1) => 5

(defun o2-nums (nums pos) ; drop nums with minority bit at pos
  (let* ((by-bit (lines-by-nth-bit pos nums))
         (nums-0 (car by-bit))
         (nums-1 (cadr by-bit)))
    (if (eq 1 (length nums)) nums (if (> (length nums-0) (length nums-1)) nums-0 nums-1))))

(defun co2-nums (nums pos) ; drop nums with majority bit at pos
  (let* ((by-bit (lines-by-nth-bit pos nums))
         (nums-0 (car by-bit))
         (nums-1 (cadr by-bit)))
    (if (eq 1 (length nums)) nums (if (<= (length nums-0) (length nums-1)) nums-0 nums-1))))
