(require 'seq)
(require 'subr-x)

(defun read-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(setq sample-input (butlast (split-string (read-file "sample.txt") "\n")))
(setq input (butlast (split-string (read-file "input.txt") "\n")))

(setq max-specpdl-size 100000)
(setq max-lisp-eval-depth 100000)

(defun bf (springs nums i ni bl mem)
  (let ((hit (gethash (list i ni bl) mem)))
    (if hit
      hit
      (progn
	(if (= i (length springs))
	  (cond
	    ((and (= ni (length nums)) (= bl 0))
	     1)
	    ((and (= ni (- (length nums) 1)) (= (nth ni nums) bl))
	     1)
	    (t
	      0))
	  (let ((result 0))
	    (dolist (c (list "." "#"))
	      (when (or (string= c (aref springs i)) (string= "?" (aref springs i)))
		(cond
		  ((and (string= "." c) (= bl 0))
		   (setq result (+ result (bf springs nums (+ i 1) ni 0 mem))))
		  ((and (string= c ".") (> bl 0) (< ni (length nums)) (= (nth ni nums) bl))
		   (setq result (+ result (bf springs nums (+ i 1) (+ ni 1) 0 mem))))
		  ((string= c "#")
		   (setq result (+ result (bf springs nums (+ i 1) ni (+ bl 1) mem)))))))
	    (puthash (list i ni bl) result mem)
	    result))))))

(defun parse-input (input)
  (let* ((pair (split-string input " "))
	 (springs (nth 0 pair))
	 (nums (mapcar 'string-to-number (split-string (nth 1 pair) ","))))
    (list springs nums)))

(defun solve1 (input)
  (seq-reduce '+ (mapcar (lambda (line)
	    (seq-let [springs nums] (parse-input line)
		     (bf (vconcat (split-string springs "" t)) nums 0 0 0 (make-hash-table :test 'equal))))
	  input) 0))

(message "%s" (solve1 sample-input)) ; 21
(message "%s" (solve1 input)) ; 7110

(defun solve2 (input)
  (seq-reduce '+ (mapcar (lambda (line)
	    (seq-let [springs nums] (parse-input line)
		     (let* ((more-nums (append (concat nums nums nums nums nums) nil))
			    (more-springs (concat springs "?" springs "?" springs "?" springs "?" springs)))
		       (bf (vconcat (split-string more-springs "" t)) more-nums 0 0 0 (make-hash-table :test 'equal)))))
	  input) 0))

(message "%s" (solve2 sample-input)) ; 525152
(message "%s" (solve2 input)) ; 1566786613613
