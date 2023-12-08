(require 'seq)
(require 'subr-x)

(defun read-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(setq sample-input
      (let* ((input (split-string (read-file "sample.txt") "\n\n"))
	     (instructions (nth 0 input))
	     (mappings (butlast (split-string (nth 1 input) "\n"))))
	(list instructions mappings)))

(setq sample-input2
      (let* ((input (split-string (read-file "sample2.txt") "\n\n"))
	     (instructions (nth 0 input))
	     (mappings (butlast (split-string (nth 1 input) "\n"))))
	(list instructions mappings)))

(setq input
      (let* ((input (split-string (read-file "input.txt") "\n\n"))
	     (instructions (nth 0 input))
	     (mappings (butlast (split-string (nth 1 input) "\n"))))
	(list instructions mappings)))

(defun string-to-list (str)
  (cdr (butlast (split-string str ""))))

(defun parse-mapping (s)
    (save-match-data
	(and (string-match "\\([0-9A-Z]+\\) = \(\\([0-9A-Z]+\\), \\([0-9A-Z]+\\)\)" s)
	    (list (match-string 1 s) (match-string 2 s) (match-string 3 s)))))

(defun build-map (input)
  (setq mp (make-hash-table :test 'equal))
  (dolist (el input)
    (let* ((parsed (parse-mapping el))
	   (from (nth 0 parsed))
	   (left (nth 1 parsed))
	   (right (nth 2 parsed)))
      (puthash from (list left right) mp)))
  mp)

(setq insts-map #s(hash-table test equal data ("L" 0 "R" 1)))

(defun solve1 (input)
  (let* ((mp (build-map (nth 1 input)))
	 (insts (string-to-list (nth 0 input)))
	 (i 0)
	 (steps 1)
	 (cur (gethash "AAA" mp))
	 (len (length insts)))
    (catch 'break
	   (while t
		  (let* ((inst (nth i insts))
			 (to-num (gethash inst insts-map))
			 (next-entry (nth to-num cur))
			 (next (gethash next-entry mp)))
		    (when (string= next-entry "ZZZ")
		      (throw 'break steps))
		    (setq cur next)
		    (setq steps (+ steps 1))
		    (setq i (mod (+ i 1) len)))))))

(message "%s" (solve1 sample-input)) ; 6
(message "%s" (solve1 input)) ; 18113

(defun do-step (from dir mp)
  (nth (gethash dir insts-map) (gethash from mp)))

(defun get-all-starts (mp)
  (seq-filter (lambda (s) (string= "A" (substring s -1 nil))) (hash-table-keys mp)))

(defun is-finish (s)
  (string= "Z" (substring s -1 nil)))

(defun is-done (lst)
  (seq-every-p #'is-finish lst))

(defun solve2 (input)
  (let* ((mp (build-map (nth 1 input)))
	 (insts (string-to-list (nth 0 input)))
	 (cur (get-all-starts mp))
	 (i 0)
	 (steps 1)
	 (len (length insts)))
    (catch 'break
	   (while t
		  (setq inst (nth i insts))
		  (setq cur (mapcar (lambda (s) (do-step s inst mp)) cur))
		  (when (is-done cur)
		    (throw 'break steps))
		    (setq steps (+ steps 1))
		    (setq i (mod (+ i 1) len))))))

(defun find-cycle (cur input)
  (let* ((mp (build-map (nth 1 input)))
	 (insts (string-to-list (nth 0 input)))
	 (i 0)
	 (steps 1)
	 (len (length insts)))
    (catch 'break
	   (while t
		  (setq inst (nth i insts))
		  (setq cur (do-step cur inst mp))
		  (when (is-finish cur)
		    (throw 'break steps))
		  (setq steps (+ steps 1))
	          (setq i (mod (+ i 1) len))))))

(defun gcd (n m)
  (cond ((< n m) (gcd m n))
        ((= m 0) n)
        (t (gcd m (% n m)))))

(defun lcm (n m)
  (/ (* n m) (gcd n m)))

(defun solve2 (input)
  (seq-reduce #'lcm (mapcar (lambda (start) (find-cycle start input)) (get-all-starts (build-map (nth 1 input)))) 1))

(message "%s" (solve2 sample-input2))
(message "%s" (solve2 input))
