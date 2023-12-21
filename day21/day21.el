(require 'seq)
(require 'subr-x)

(defun read-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun convert-to-array (input)
  (vconcat (mapcar (lambda (line) (vconcat line nil)) input) nil))

(setq sample-input (convert-to-array (mapcar (lambda (line) (split-string line "" t)) (butlast (split-string (read-file "sample.txt") "\n")))))
(setq input (convert-to-array (mapcar (lambda (line) (split-string line "" t)) (butlast (split-string (read-file "input.txt") "\n")))))

(defun calc (sr sc steps m)
  (setq res (make-hash-table :test 'equal))
  (setq been (make-hash-table :test 'equal))
  (puthash (list sr sc) t been)
  (setq q (list (list sr sc steps)))
  (setq R (length m))
  (setq C (length (aref m 0)))

  (while (> (length q) 0)
	 (seq-let (r c s) (pop q)
		  (when (= (mod s 2) 0)
		    (puthash (list r c) t res))
		  (when (not (= s 0))
		    (dolist (dir (list (list 0 1) (list 0 -1) (list 1 0) (list -1 0)))
		      (seq-let (dr dc) dir
			       (let ((nr (+ r dr))
				     (nc (+ c dc)))
				 (when (and (>= nr 0) (< nr R) (>= nc 0) (< nc C) (not (string= (aref (aref m nr) nc) "#")))
				   (when (not (gethash (list nr nc) been))
				     (puthash (list nr nc) t been)
				     (setq q (append q (list (list nr nc (- s 1)))))))))))))
  (length (hash-table-keys res)))

(message "%s" (calc 5 5 6 sample-input)) ; 16
(message "%s" (calc 65 65 64 input)) ; 3788

(setq R (length input))
(setq steps 26501365)
(setq size (/ steps R))

(setq odd-tiles (expt (+ 1 (* (/ (- size 1) 2) 2)) 2))
(setq even-tiles (expt (* (/ size 2) 2) 2))

(setq los (* R 2)) ; just a lot of steps
(setq odd (* odd-tiles (calc 65 65 (+ los 1) input)))
(setq even (* even-tiles (calc 65 65 los input)))

(setq sbt (- R 1)) ; steps bottom to top
(setq leftovers1 (+
		   (calc 65 0 sbt input)
		   (calc 0 65 sbt input)
		   (calc (- R 1) 65 sbt input)
		   (calc 65 (- R 1) sbt input)))

(setq shw (- (/ R 2) 1)) ; steps half the way
(setq leftovers2 (+ 
		   (calc (- R 1) 0 shw input)
		   (calc (- R 1) (- R 1) shw input)
		   (calc 0 0 shw input)
		   (calc 0 (- R 1) shw input)))
(setq leftovers2 (* size leftovers2))

(setq sae (- (/ (* R 3) 2) 1)) ; steps to almost the end
(setq leftovers3 (+
		   (calc (- R 1) 0 sae input)
		   (calc (- R 1) (- R 1) sae input)
		   (calc 0 0 sae input)
		   (calc 0 (- R 1) sae input)))
(setq leftovers3 (* (- size 1) leftovers3))

(message "%s" (+ odd even leftovers1 leftovers2 leftovers3)) ; 631357596621921
