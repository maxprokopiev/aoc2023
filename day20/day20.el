(require 'seq)
(require 'subr-x)

(defun read-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(setq sample-input (butlast (split-string (read-file "sample.txt") "\n")))
(setq sample-input2 (butlast (split-string (read-file "sample2.txt") "\n")))
(setq input (butlast (split-string (read-file "input.txt") "\n")))

(defun init-conjs (m)
  (setq keys (hash-table-keys m))
  (dolist (m-name keys)
    (let* ((module (gethash m-name m))
	   (typ (nth 0 module))
	   (state (nth 1 module)))
      (when (string= typ "&")
	(dolist (m-name2 keys)
	  (let* ((ms (car (last (gethash m-name2 m)))))
	    (when (seq-position ms m-name)
	      (puthash m-name2 nil state))))
	(puthash m-name (list typ state (car (last module))) m))))
  m)

(defun parse-input (input)
  (let* ((m (make-hash-table :test 'equal))
	 (rules (mapcar (lambda (line)
			  (let* ((spl (split-string line " -> " t))
				 (name-with-type (nth 0 spl))
				 (modules (split-string (nth 1 spl) ", " t)))
			    (if (string= name-with-type "broadcaster")
			      (progn
			        (puthash name-with-type (list "b" nil modules) m))
			      (progn
				(let* ((name (substring name-with-type 1))
				       (typ (substring name-with-type 0 1)))
				  (cond
				    ((string= typ "%")
				     (puthash name (list typ nil modules) m))
				    ((string= typ "&")
				     (puthash name (list typ (make-hash-table :test 'equal) modules) m))
				    )))))) input)))
    (init-conjs m)))

(defun update-module-state (to from pulse m)
  (let* ((state-l (gethash to m))
         (typ (nth 0 state-l))
         (state (nth 1 state-l))
         (modules (nth 2 state-l)))
    (cond
      ((string= typ "%")
        (when (not pulse)
          (setq state (not state))))
      ((string= typ "&")
        (puthash from pulse state))
      (t
        (setq state pulse)))
    (puthash to (list typ state modules) m)
    m))

(defun get-pulse (module m)
  (let* ((state-l (gethash module m))
         (typ (nth 0 state-l))
         (state (nth 1 state-l)))
    (cond
      ((string= typ "%")
        state)
      ((string= typ "&")
        (if (seq-every-p 'identity (hash-table-values state)) nil t))
      (t
        state))))

(defun press (to-count m)
  (let ((lc 0)
        (hc 0)
        (cycle 0)
        (q (list (list "button" "broadcaster" nil))))
    (while (> (length q) 0)
      (let* ((event (pop q))
             (from (nth 0 event))
             (to (nth 1 event))
             (pulse (nth 2 event))
	     (typ (nth 0 (gethash to m))))
        (if pulse
          (setq hc (+ hc 1))
          (setq lc (+ lc 1)))
        (if (and (string= to to-count) (not pulse))
          (setq cycle (+ cycle 1)))
        (update-module-state to from pulse m)
        (when (not (and (string= typ "%") pulse))
	  (let ((np (get-pulse to m))
                (modules (car (last (gethash to m)))))
            (dolist (module modules)
              (setq q (append q (list (list to module np)))))))))
    (list lc hc cycle)))

(defun solve1 (m)
  (let ((ll 0)
	(hh 0))
    (dotimes (i 1000)
      (let* ((l (press "" m))
    	 (lc (nth 0 l))
    	 (hc (nth 1 l)))
        (setq ll (+ ll lc))
        (setq hh (+ hh hc))))
    (* ll hh)))

(message "%s" (solve1 (parse-input sample-input))) ; 11687500
(message "%s" (solve1 (parse-input sample-input2))) ; 32000000
(message "%s" (solve1 (parse-input input))) ; 743871576

(defun gcd (n m)
  (cond ((< n m) (gcd m n))
        ((= m 0) n)
        (t (gcd m (% n m)))))

(defun lcm (n m)
  (/ (* n m) (gcd n m)))

(defun solve2 (connections input)
  (seq-reduce 'lcm (mapcar (lambda (module)
	    (let ((m (parse-input input))
		  (bp 0)
		  (r 0))
	      (while (not (= r 1))
		     (setq bp (+ bp 1))
		     (setq r (nth 2 (press module m))))
	      bp)) connections) 1))

(message "%s" (solve2 (list "ph" "vn" "kt" "hn") input)) ; ; 244151741342687
