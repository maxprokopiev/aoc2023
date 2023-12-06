(require 'seq)

(defun read-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(setq sample-input (butlast (split-string (read-file "sample.txt") "\n")))
(setq input (butlast (split-string (read-file "input.txt") "\n")))

(defun get-times (input)
  (let ((time-line (nth 0 input)))
    (when (string-match "Time: \\(.+\\)" time-line)
      (remove 0 (mapcar #'string-to-number (split-string (match-string 1 time-line) " "))))))

(defun get-distances (input)
  (let ((distance-line (nth 1 input)))
    (when (string-match "Distance: \\(.+\\)" distance-line)
      (remove 0 (mapcar #'string-to-number (split-string (match-string 1 distance-line) " "))))))

(defun get-distance (hold-time total-time)
  (* hold-time (- total-time hold-time)))

(defun get-number-of-ways (race-time record-distance)
  (length (seq-filter (lambda (el) (> el record-distance)) (mapcar (lambda (tt) (get-distance tt race-time)) (number-sequence 1 race-time)))))

(defun mapcar* (f &rest xs)
  (if (not (memq nil xs))
    (cons (apply f (mapcar 'car xs))
      (apply 'mapcar* f (mapcar 'cdr xs)))))

(defun solve1 (input)
  (let ((td (mapcar* #'cons (get-times input) (get-distances input))))
    (seq-reduce #'* (mapcar (lambda (pair) (get-number-of-ways (car pair) (cdr pair))) td) 1)))

; (message "%s" (solve1 sample-input)) ; 288
; (message "%s" (solve1 input)) ; 345015

(message "%s" (get-number-of-ways 71530 940200)) ; 71503
(message "%s" (get-number-of-ways 60947882 475213810151650)) ; 42588603

