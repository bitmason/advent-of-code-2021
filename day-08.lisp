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
        for entry = (mapcar #'words (split "|" line))
        for signal-patterns = (mapcar (lambda (s) (coerce s 'list))
                                      (first entry))  ; e.g. ((#\a \c) (#\f \#b #\d) ... )
        for output-digits = (mapcar (lambda (s) (coerce s 'list))
                                    (second entry))   ; as above
        sum (output-value signal-patterns output-digits)))

(defun output-value (sig-pats out-digs)  ; output value (e.g. 5623)
  (let* ((dig-segs (make-array '(10) :initial-element nil)))  ; deduced segs for each dig
    (loop for dp in ; each digit has a deductive predicate
          `((1 ,(lambda (segs) (= (length segs) 2)))  ; unique seg count
            (4 ,(lambda (segs) (= (length segs) 4)))  ; unique seg count
            (7 ,(lambda (segs) (= (length segs) 3)))  ; unique seg count
            (8 ,(lambda (segs) (= (length segs) 7)))  ; unique seg count
            (3 ,(lambda (segs) (and (= (length segs) 5)  ; 5 segs incl "1"
                                   (subsetp (aref dig-segs 1) segs))))
            (9 ,(lambda (segs) (and (= 6 (length segs))  ; 6 segs incl "3"
                                   (subsetp (aref dig-segs 3) segs))))
            (0 ,(lambda (segs) (and (= 6 (length segs))  ; 6 segs, not "9", incl "1"
                                   (set-exclusive-or segs (aref dig-segs 9))
                                   (subsetp (aref dig-segs 1) segs))))
            (2 ,(lambda (segs) (and (= 5 (length segs))  ; 5 segs, 3 segs diff than "4"
                                   (= 3 (length (set-difference segs (aref dig-segs 4)))))))
            (5 ,(lambda (segs) (and (= 5 (length segs))  ; 5 segs & not yet classified
                                   (not (segs-pos segs dig-segs)))))
            (6 ,(lambda (segs) (and (= 6 (length segs))  ; 6 segs & adding "1" makes 7 segs
                                   (= 7 (length (union segs (aref dig-segs 1))))))))
          do (set-dig (car dp) dig-segs sig-pats (second dp))) ; deduce all to fill dig-segs
    (loop for out-segs in out-digs
          for place in '(1000 100 10 1)
          sum (* (segs-pos out-segs dig-segs)  ; find digit represented by out-segs
                 place))))

;; position of segs pattern in dig-segs array (or nil)
(defun segs-pos (segs dig-segs) (position segs dig-segs :test-not #'set-exclusive-or))

;; set dig in dig-segs array to 1st sig-pat satisfying predicate
(defun set-dig (dig dig-segs sig-pats pred)
  (setf (aref dig-segs dig)
        (car (remove-if-not pred sig-pats))))
