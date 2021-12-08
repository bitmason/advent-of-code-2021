;;;; Advent of Code 2021, Day 8
;;;; https://adventofcode.com/2021/day/8
;;;; Solution by Darren Stone <dstone@bitmason.com>

(defun day-08 (input) (list (day-08-part-1 input)
                            (day-08-part-2 input)))

(defun day-08-part-1 (input)
  (loop for line in input
        for entry = (mapcar #'words (split "|" line))
        for output-digits = (second entry)  ; e.g. ("bcgaf" "edf" ...)
        sum (length (remove-if-not          ; only these unique seg counts
                     (lambda (segs) (member (length segs) '(2 4 3 7)))
                     output-digits))))

(defun day-08-part-2 (input)
  (loop for line in input
        ;; for entry = (mapcar (lambda (s) (coerce s 'list))
        ;;                     (mapcar #'words (split "|" line)))
        ;; for signal-patterns = (first entry)  ; e.g. ((#\a \c) (#\f \#b #\d) ... )
        ;; for output-digits = (second entry)   ; as above
        for entry = (mapcar #'words (split "|" line))
        for signal-patterns = (mapcar (lambda (s) (coerce s 'list))
                                      (first entry))  ; e.g. ((#\a \c) (#\f \#b #\d) ... )
        for output-digits = (mapcar (lambda (s) (coerce s 'list))
                                    (second entry))   ; as above
        sum (output-value signal-patterns output-digits)))

(defun output-value (signal-patterns output-digits)  ; output value (e.g. 5623)
  (let* ((dig-segs (make-array '(10) :initial-element nil)))  ; deduced segments for each digit
    (loop for digit in '(1 4 7 8)  ; unique seg count
          for seg-count in '(2 4 3 7)
          do (setf (aref dig-segs digit)
                   (car (remove-if-not
                         (lambda (segs) (= (length segs) seg-count))
                         signal-patterns))))
    (setf (aref dig-segs 3)  ; 5 segs incl those for dig 1
          (car (remove-if-not
                (lambda (segs) (and (= 5 (length segs))
                                    (subsetp (aref dig-segs 1) segs)))
                signal-patterns)))
    (setf (aref dig-segs 9)  ; 6 segs incl those for dig 3
          (car (remove-if-not
                (lambda (segs) (and (= 6 (length segs))
                                    (subsetp (aref dig-segs 3) segs)))
                signal-patterns)))
    (setf (aref dig-segs 0)  ; 6 segs (but not dig 9) incl those for dig 1
          (car (remove-if-not
                (lambda (segs) (and (set-exclusive-or segs (aref dig-segs 9))
                                    (= 6 (length segs))
                                    (subsetp (aref dig-segs 1) segs)))
                signal-patterns)))
    (setf (aref dig-segs 2)  ; 5 segs with 3 segs diff than dig 4
          (car (remove-if-not
                (lambda (segs)
                  (and (= 5 (length segs))
                       (= 3 (length (set-difference segs (aref dig-segs 4))))))
                signal-patterns)))
    (setf (aref dig-segs 5)  ; 5 segs and not yet classified
          (car (remove-if-not
                (lambda (segs)
                  (and (= 5 (length segs))
                       (not (position segs dig-segs :test-not #'set-exclusive-or))))
                signal-patterns)))
    (setf (aref dig-segs 6)  ; 6 segs and adding segs for dig 1 makes 7 segs
          (car (remove-if-not
                (lambda (segs)
                  (and (= 6 (length segs))
                       (= 7 (length (union segs (aref dig-segs 1))))))
                signal-patterns)))
    (loop for out-segs in output-digits
          for place in '(1000 100 10 1)
          sum (* (position out-segs  ; find digit (dig-segs index) represented by out-segs
                        dig-segs
                        :test-not #'set-exclusive-or)
                 place))))
