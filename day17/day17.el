(require 'seq)
(require 'subr-x)

(load-file (expand-file-name "heap.el" (file-name-directory ".")))

(defun read-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(setq sample-input (mapcar (lambda (line) (mapcar 'string-to-number (split-string line "" t))) (butlast (split-string (read-file "sample.txt") "\n"))))
(setq input (mapcar (lambda (line) (mapcar 'string-to-number (split-string line "" t))) (butlast (split-string (read-file "input.txt") "\n"))))

(defun d (max-steps min-steps input)
  (setq R (length input))
  (setq C (length (nth 0 input)))
  (setq q (make-heap (lambda (a b) (< (car a) (car b)))))
  (heap-add q (list 0 0 0 0 0 0))
  (setq been (make-hash-table :test 'equal))

  (catch 'break
	 (while (not (heap-empty q))
		(catch 'continue
		       (let* ((popped (heap-delete-root q))
			      (dist (nth 0 popped))
			      (r (nth 1 popped))
			      (c (nth 2 popped))
			      (dr (nth 3 popped))
			      (dc (nth 4 popped))
			      (steps (nth 5 popped)))
			 (when (and (= r (- R 1)) (= c (- C 1)) (>= steps min-steps))
			   (throw 'break dist))
			 (if (gethash (cdr popped) been)
			   (throw 'continue nil))
			 (puthash (cdr popped) t been)
			 (when (and (< steps max-steps) (not (equal (list dr dc) (list 0 0))))
			   (let ((nr (+ r dr))
				 (nc (+ c dc)))
			     (when (and (>= nr 0) (< nr R) (>= nc 0) (< nc C))
			       (heap-add q (list (+ dist (nth nc (nth nr input))) nr nc dr dc (+ steps 1))))))
			 (when (or (equal (list dr dc) (list 0 0)) (>= steps min-steps))
			   (dolist (dir (list (list 0 1) (list 0 -1) (list 1 0) (list -1 0)))
			     (let* ((ndr (nth 0 dir))
			  	  (ndc (nth 1 dir)))
			       (when (and (not (equal (list (- dr) (- dc)) (list ndr ndc))) (not (equal (list dr dc) (list ndr ndc))))
			         (let ((nr (+ r ndr))
			  	     (nc (+ c ndc)))
			  	 (when (and (>= nr 0) (< nr R) (>= nc 0) (< nc C))
			  	   (heap-add q (list (+ dist (nth nc (nth nr input))) nr nc ndr ndc 1)))))))))))))

(message "%s" (d 3 0 sample-input)) ; 102
(message "%s" (d 3 0 input)) ; 1138
(message "%s" (d 10 4 sample-input)) ; 94
(message "%s" (d 10 4 input)) ; 1312
