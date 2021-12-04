;;;; Advent of Code 2021, Day 4
;;;; https://adventofcode.com/2021/day/4
;;;; Solution by Darren Stone <dstone@bitmason.com>

(defun day-04-part-1 (input) (cadr (first (all-wins input)))) ; score of first winning board

(defun day-04-part-2 (input) (cadar (last (all-wins input)))) ; score of last winning board

(defun all-wins (input) ; list of all wins in drawn order: (board score) pairs
  (let ((deck (mapcar #'parse-integer (split "," (car input)))))
    (remove-duplicates (loop for drawn in (loop for n from 1 to (length deck) collect (subseq deck 0 n))
                             append (loop for board in (bingo-boards input)
                                          when (board-wins-p board drawn)
                                            collect (list board (board-score board drawn))))
                       :from-end t :test (lambda (a b) (equal (car a) (car b))))))

(defun bingo-boards (input) ; list of 25-element lists of integers
  (let* ((flat (mapcar #'parse-integer (words (join " " (cdr input)))))
         (num-boards (/ (length flat) 25)))
    (loop for n below num-boards collect (subseq flat (* n 25) (+ 25 (* n 25))))))

(defun board-wins-p (board drawn) ; given drawn numbers, board wins?
  (let* ((lines (append (loop for r in '(0 5 10 15 20) collect (loop for v from r to (+ r 4) collect v))
                        (loop for c in '(0 1 2 3 4) collect (loop for v from c to (+ c 20) by 5 collect v))))
         (line-nums (loop for line in lines collect (mapcar (lambda (pos) (nth pos board)) line))))
    (some (lambda (line) (subsetp line drawn)) line-nums)))

(defun board-score (board drawn) ; score of given winning board and numbers drawn first to last
  (* (car (last drawn)) (reduce #'+ (set-difference board drawn))))
