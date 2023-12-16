(require 'seq)
(require 'subr-x)

(defun read-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun print-list (input)
  (let* ((row-count (length input))
	 (column-count (length (nth 0 input))))
    (dotimes (row row-count)
      (dotimes (column column-count)
	(let ((el (nth column (nth row input))))
	  (princ (format "%s " el))))
      (princ "\n"))))

(defun print-array (input)
  (let* ((row-count (length input))
	 (column-count (length (aref input 0))))
    (dotimes (row row-count)
      (dotimes (column column-count)
	(let ((el (aref (aref input row) column)))
	  (princ (format "%s " el))))
      (princ "\n"))))

(defun print-str-list (input)
  (let* ((row-count (length input)))
    (dotimes (row row-count)
      (princ (nth row input))
      (princ "\n"))))

(defun convert-to-array (input)
  (vconcat (mapcar (lambda (line) (vconcat line nil)) input) nil))

(setq sample-input (mapcar (lambda (line) (split-string line "" t)) (butlast (split-string (read-file "sample.txt") "\n"))))
(setq input (mapcar (lambda (line) (split-string line "" t)) (butlast (split-string (read-file "input.txt") "\n"))))

(defun move (coord dir)
  (list (+ (nth 0 coord) (nth 0 dir)) (+ (nth 1 coord) (nth 1 dir))))

(setq up (list 0 -1))
(setq down (list 0 1))
(setq right (list 1 0))
(setq left (list -1 0))

(defun get-nd-backslash (dir)
  (cond
    ((equal dir up)
     left)
    ((equal dir down)
     right)
    ((equal dir right)
     down)
    ((equal dir left)
     up)))

(defun get-nd-slash (dir)
  (cond
    ((equal dir up)
     right)
    ((equal dir down)
     left)
    ((equal dir right)
     up)
    ((equal dir left)
     down)))

(defun solve (starts input)
  (let* ((been (make-hash-table :test 'equal))
	 (seen nil)
	 (l starts)
	 (len (length input))
	 (len2 (length (aref input 0))))
    (while (> (length l) 0)
	   (let* ((beam (pop l))
		  (coord (nth 0 beam))
		  (dir (nth 1 beam))
		  (nc (move coord dir))
		  (lkup (seq-position seen (list nc dir)))
		  (ncx (nth 0 nc))
		  (ncy (nth 1 nc)))
	     (when (and (not lkup) (>= ncy 0) (>= ncx 0) (< ncy len) (< ncx len2))
	       (let ((tile (aref (aref input ncy) ncx)))
	        (push (list nc dir) seen)
		(puthash nc t been)
	         (cond
	           ((string= tile ".")
	            (push (list nc dir) l))
	           ((string= tile "|")
		    (cond
		      ((or (equal dir down) (equal dir up))
		       (push (list nc dir) l))
		      (t
			(push (list nc up) l)
			(push (list nc down) l))))
	           ((string= tile "-")
		    (cond
		      ((or (equal dir right) (equal dir left))
		       (push (list nc dir) l))
		      (t
			(push (list nc left) l)
			(push (list nc right) l))))
		   ((string= tile "\\")
		    (let ((nd (get-nd-backslash dir)))
		      (push (list nc nd) l)))
		   ((string= tile "/")
		    (let ((nd (get-nd-slash dir)))
		      (push (list nc nd) l))))))))
    (length (hash-table-keys been))))


(defun solve2 (input)
  (let* ((seq (number-sequence 0 (- (length input) 1)))
         (a1 (mapcar (lambda (el) (list (list -1 el) right)) seq))
         (a2 (mapcar (lambda (el) (list (list el -1) down)) seq))
         (a3 (mapcar (lambda (el) (list (list (length input) el) left)) seq))
         (a4 (mapcar (lambda (el) (list (list el (length input)) up)) seq))
	 (arr (convert-to-array input)))
    (apply 'max (mapcar (lambda (s) (solve (list s) arr)) (append a1 a2 a3 a4 nil)))))

(message "%s" (solve (list (list (list -1 0) right)) (convert-to-array sample-input))) ; 46
(message "%s" (solve (list (list (list -1 0) right)) (convert-to-array input))) ; 8146

(message "%s" (solve2 sample-input)) ; 51
(message "%s" (solve2 input)) ; 8358

