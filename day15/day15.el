(require 'seq)
(require 'subr-x)

(defun read-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(setq sample-input (butlast (split-string (read-file "sample.txt") "\n")))
(setq input (butlast (split-string (read-file "input.txt") "\n")))

(defun remove-nth-element (nth list)
  (if (zerop nth) (cdr list)
    (let ((last (nthcdr (1- nth) list)))
      (setcdr last (cddr last))
      list)))

(defun to-hash (s)
  (seq-reduce (lambda (acc c)
		(% (* (+ acc c) 17) 256)) s 0))

(defun solve1 (input)
  (let* ((insts (split-string (car input) "," t)))
    (seq-reduce '+ (mapcar 'to-hash insts) 0)))

(defun parse-inst (inst)
  (if (seq-contains inst (string-to-char "="))
    (let* ((parts (split-string inst "=" t))
	   (label (nth 0 parts))
	   (focus (string-to-number (nth 1 parts)))
	   (hash (to-hash label)))
      (list label "=" hash focus))
    (let* ((parts (split-string inst "-" t))
	   (label (nth 0 parts))
	   (hash (to-hash label)))
      (list label "-" hash))))


(message "%s" (solve1 sample-input)) ; 1320
(message "%s" (solve1 input)) ; 505379

(defun solve2 (input)
  (let* ((insts (split-string (car input) "," t))
	 (h (make-hash-table :test 'equal)))
    (dolist (inst insts)
      (let* ((parts (parse-inst inst))
	     (label (nth 0 parts))
	     (op (nth 1 parts))
	     (hash (nth 2 parts))
	     (b (gethash hash h)))
	(cond
	  ((string= op "=")
	   (let ((pos (seq-position b label (lambda (e lbl) (string= (nth 0 e) lbl)))))
	     (if pos
	       (setcar (nthcdr pos b) (list label (nth 3 parts)))
	       (setq b (append b (list (list label (nth 3 parts))))))
	     (puthash hash b h)))
	  ((string= op "-")
	   (let ((pos (seq-position b label (lambda (e lbl) (string= (nth 0 e) lbl)))))
	     (when pos
	       (puthash hash (remove-nth-element pos b) h))))
	   )))
    (seq-reduce '+ (mapcar (lambda (bn)
	      (let* ((l (gethash bn h))
		     (l-ind (seq-map-indexed (lambda (e ind) (list e (+ ind 1))) l)))
		(seq-reduce (lambda (acc pair)
				 (+ acc (* (+ bn 1) (nth 1 pair) (nth 1 (nth 0 pair))))) l-ind 0)))
	    (hash-table-keys h)) 0)))


(message "%s" (solve2 sample-input)) ; 145
(message "%s" (solve2 input)) ; 263211
