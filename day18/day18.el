(require 'seq)
(require 'subr-x)

(defun read-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(setq sample-input (butlast (split-string (read-file "sample.txt") "\n")))
(setq input (butlast (split-string (read-file "input.txt") "\n")))

(setq h #s(hash-table test equal data ("0" "R" "1" "D" "2" "L" "3" "U")))
(setq l2d #s(hash-table test equal data ("R" (0 1) "D" (1 0) "L" (0 -1) "U" (-1 0))))

(defun parse-input (input)
  (mapcar (lambda (line)
	    (let* ((spl (split-string line " " t))
		   (dir (nth 0 spl))
		   (steps (string-to-number (nth 1 spl))))
	      (list dir steps))) input))

(defun parse-input-2 (input)
  (mapcar (lambda (line)
	    (let* ((spl (split-string line " " t))
		   (col (car (last spl)))
		   (steps (string-to-number (substring col 2 7) 16))
		   (dir (gethash (substring col -2 -1) h)))
	      (list dir steps))) input))

(defun p (input)
  (seq-reduce '+ (mapcar (lambda (el) (nth 1 el)) input) 0))

(defun build-coords (input)
  (setq coords (list (list 0 0)))
  (dolist (inst input)
    (let* ((steps (nth 1 inst))
	   (prev (car coords))
	   (r (nth 0 prev))
	   (c (nth 1 prev))
	   (dir (gethash (nth 0 inst) l2d))
	   (dr (nth 0 dir))
	   (dc (nth 1 dir)))
      (push (list (+ r (* dr steps)) (+ c (* dc steps))) coords)))
  (butlast coords))

(defun a (c)
  (setq r 0)
  (seq-map-indexed (lambda (el i)
		     (if (< (+ i 1) (length c))
		       (let ((p2 (nth (+ i 1) c)))
			 (setq r (+ r (-
					(* (nth 1 el) (nth 0 p2))
					(* (nth 1 p2) (nth 0 el))))))
		       (setq r (+ r (-
				      (* (nth 1 el) (nth 0 (car c)))
				      (* (nth 1 (car c)) (nth 0 el))))))) c)
  (/ r 2))

(defun solve (parsed)
  (setq p (p parsed))
  (setq coords (build-coords parsed))
  (+ (/ p 2) (abs (a coords)) 1))

(message "%s" (solve (parse-input sample-input))) ; 62
(message "%s" (solve (parse-input input))) ; 35244
(message "%s" (solve (parse-input-2 sample-input))) ; 952408144115
(message "%s" (solve (parse-input-2 input))) ; 85070763635666
