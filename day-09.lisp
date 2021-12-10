;;;; Advent of Code 2021, Day 9
;;;; https://adventofcode.com/2021/day/9
;;;; Solution by Darren Stone <dstone@bitmason.com>

(defun day-09 (input)
  (let* ((y-size (length input))
         (x-size (length (car input)))
         (heightmap (make-array (list y-size x-size))))

    (loop for line in input  ; read heightmap
          for y below y-size
          do (loop for h in (coerce line 'list)
                   for x below x-size
                   do (setf (aref heightmap y x)
                            (parse-integer (string h)))))

    (list (day-09-part-1 heightmap)
          (day-09-part-2 heightmap))))

(defun day-09-part-1 (hm)
  (loop for lp in (low-points hm)
        sum (+ 1 (aref hm (first lp) (second lp)))))

(defun day-09-part-2 (hm)
  (apply #'* (subseq (sort (mapcar (lambda (p) (length (basin-points hm p)))
                                   (low-points hm))
                           #'>)
                     0 3)))

(defun low-points (hm)  ; list of (y x) low points in heightmap
  (loop for y below (array-dimension hm 0)
        append (loop for x below (array-dimension hm 1)
                    for h = (aref hm y x)
                    when (< h (reduce #'min (adj-heights hm y x)))
                      collect (list y x))))

(defun basin-points (hm lp)  ; all (y x) in heightmap that flow down to low-point
  (let ((points (list lp)))
    (loop
      for h from (aref hm (first lp) (second lp)) to 8  ; sweep for increasing heights
      append (loop
               for y below (array-dimension hm 0)
               append (loop
                        for x below (array-dimension hm 1)
                        append (loop
                                 for adj in (adj-points hm y x)  ; check adj points
                                 when (and (= h (aref hm y x))   ; with current height
                                           (not (position (list y x) points  ; no dupes
                                                          :test #'eq-yx))
                                           (position adj points  ; connected (i.e. basin)
                                                     :test #'eq-yx))
                                   do (push (list y x) points)))))
    points))

(defun adj-points (hm y x)  ; list of (y x) adjacent points in heightmap
  (loop for off in '((-1 0) (0 1) (1 0) (0 -1))
        for ny = (+ y (car off))
        for nx = (+ x (second off))
        when (and (>= ny 0)
                  (< ny (array-dimension hm 0))
                  (>= nx 0)
                  (< nx (array-dimension hm 1)))
          collect (list ny nx)))

(defun adj-heights (hm y x)  ; list of 2-4 neighbour heights
  (mapcar (lambda (p) (aref hm (first p) (second p)))
          (adj-points hm y x)))

(defun eq-yx (y1x1 y2x2)  ; equality for (y1 x1) =?= (y2 x2)
  (and (= (first y1x1) (first y2x2))
       (= (second y1x1) (second y2x2))))
