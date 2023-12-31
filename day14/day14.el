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

(defun rotate-left (input)
  (let ((row-count (length input))
        (col-count (length (aref input 0)))
        result)
    (setq result (make-matrix col-count row-count 1))
    (dotimes (r row-count)
      (dotimes (c col-count)
	(let ((el (aref (aref input r) c)))
	  (aset (aref result (- (length input) c 1)) r el))))
    result))

(defun rotate-right (input)
  (let ((row-count (length input))
        (col-count (length (aref input 0)))
        result)
    (setq result (make-matrix col-count row-count 1))
    (dotimes (r row-count)
      (dotimes (c col-count)
	(let ((el (aref (aref input r) c)))
	  (aset (aref result c) (- row-count r 1) el))))
    result))

(setq sample-input (mapcar (lambda (line) (split-string line "" t)) (butlast (split-string (read-file "sample.txt") "\n"))))
(setq input (mapcar (lambda (line) (split-string line "" t)) (butlast (split-string (read-file "input.txt") "\n"))))

(defun move (li input)
  (let* ((prev-line (aref input (- li 1)))
	 (line (aref input li)))
    (dotimes (i (length input))
      (when (and (string= (aref line i) "O") (string= (aref prev-line i) "."))
	(aset line i ".")
	(aset prev-line i "O")))
    (aset input (- li 1) prev-line)
    (aset input li line)
    input))

(defun fall (input)
  (let ((result input))
    (dotimes (i (- (length input) 1))
      (let* ((li (+ i 1)))
	(dotimes (ai li)
          (setq result (move (- li ai) input)))))
    result))

(defun solve1 (input)
  (seq-reduce '+ (seq-map-indexed (lambda (line i)
		   (* (+ i 1) (length (seq-filter (lambda (el) (string= el "O"))
			       line))))
		       (reverse input)) 0))

(message "%s" (solve1 (fall (convert-to-array sample-input)))) ; 136
(message "%s" (solve1 (fall (convert-to-array input)))) ; 108792

(defun cycle (input)
  (let* ((north (fall input))
	 (west (fall (rotate-right north)))
	 (south (fall (rotate-right west)))
	 (east (fall (rotate-right south))))
    (rotate-right east)))

(defun arr-to-str (arr)
  (let ((s ""))
    (dolist (el (append arr nil))
      (setq s (concat s (mapconcat 'identity el ""))))
    s))

(defun solve2 (input)
  (setq r (convert-to-array input))
  (setq seen (make-hash-table :test 'equal))
  (setq num 1000000000)
  (setq i 0)

  (while (< i num)
         (setq i (+ i 1))
         (let* ((next (cycle r))
  	      (astr (arr-to-str next))
  	      (hsh (gethash astr seen)))
  	 (when hsh
  	   (let ((cycle-length (- i hsh)))
  	     (setq i (+ i (* (floor (/ (- num i) cycle-length)) cycle-length)))))
  	 (puthash astr i seen)
  	 (setq r next)))
  (solve1 r))

(message "%s" (solve2 sample-input)) ; 64
(message "%s" (solve2 input)) ; 99118
