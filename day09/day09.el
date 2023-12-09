(require 'seq)
(require 'subr-x)

(defun read-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(setq sample-input (butlast (split-string (read-file "sample.txt") "\n")))
(setq input (butlast (split-string (read-file "input.txt") "\n")))

(defun do-round (h)
  (seq-map-indexed (lambda (el i) (- (nth (+ i 1) h) el)) (butlast h)))

(defun get-lists (cur lists)
  (let* ((next (do-round cur)))
    (push next lists)
    (if (seq-every-p (lambda (el) (= el 0)) next)
      lists
      (get-lists next lists))))

(defun get-next (h)
  (seq-reduce '+ (mapcar (lambda (l) (car (last l))) (get-lists h (list h))) 0))

(defun get-next2 (h)
  (seq-reduce (lambda (el acc) (- acc el)) (mapcar (lambda (l) (car l)) (get-lists h (list h))) 0))

(defun solve (fn input)
  (let* ((h (mapcar (lambda (l) (mapcar 'string-to-number (split-string l " "))) input)))
    (seq-reduce '+ (mapcar fn h) 0)))

(message "%s" (solve 'get-next sample-input)) ; 114
(message "%s" (solve 'get-next input)) ; 2098530125

(message "%s" (solve 'get-next2 sample-input)) ; 22
(message "%s" (solve 'get-next2 input)) ; 1016
