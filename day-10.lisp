;;;; Advent of Code 2021, Day 10
;;;; https://adventofcode.com/2021/day/10
;;;; Solution by Darren Stone <dstone@bitmason.com>

(defun day-10 (input)
  (loop for line in input
        with close-points = (list #\( #\[ #\{ #\<)
        for x = (check (coerce line 'list))
        if (numberp x)  ; corruption score
          sum x into part-1
        else  ; open chars e.g. (#\( #\< #\<)
          collect (reduce (lambda (score char)
                            (+ (* 5 score)
                               (+ 1 (position char close-points))))
                          x
                          :initial-value 0) into part-2-scores
        finally (return (list part-1 (middle part-2-scores)))))

(defun middle (scores)  ; sort and return middle score
  (nth (floor (length scores) 2)
       (sort scores #'<)))
;;
;; corruption points iff valid closing char
(defun close-p (c) (getf '(#\) 3 #\] 57 #\} 1197 #\> 25137) c nil))

(defun open-close (c) (getf '(#\( #\) #\[ #\] #\{ #\} #\< #\>) c nil))

; corruption score if corrupt, otherwise list of open chars to complete
(defun check (line &key open-chars)
  (let* ((c (car line))
         (close (close-p c)))
    (if (not line)
        open-chars ; if open-chars then we have an incomplete line (ignore)
        (if close
            (if (eq c (open-close (car open-chars)))
                (check (cdr line) :open-chars (cdr open-chars))  ; balanced pair; continue
                close)  ; corrupt line
            (check (cdr line) :open-chars (cons c open-chars))))))  ; newly opened chunk
