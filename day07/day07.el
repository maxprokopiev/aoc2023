(require 'seq)
(require 'subr-x)

(defun read-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(setq sample-input (butlast (split-string (read-file "sample.txt") "\n")))
(setq input (butlast (split-string (read-file "input.txt") "\n")))

(defun get-cards-and-bid (input)
  (mapcar (lambda (line) (split-string line " ")) input))

(defun count-chars (card)
  (setq counts (make-hash-table :test 'equal))
  (dolist (chr (append card nil))
    (let ((cnt (gethash (char-to-string chr) counts)))
      (if cnt
	(puthash (char-to-string chr) (+ 1 cnt) counts)
	(puthash (char-to-string chr) 1 counts))))
  counts)

(defun get-type (card-with-counts)
  (let ((counts (hash-table-values card-with-counts)))
    (let ((sorted (sort counts '<)))
      (cond
        ((equal sorted '(5))
	 7)
        ((equal sorted '(1 4))
	 6)
        ((equal sorted '(2 3))
	 5)
        ((equal sorted '(1 1 3))
	 4)
        ((equal sorted '(1 2 2))
	 3)
        ((equal sorted '(1 1 1 2))
	 2)
        ((equal sorted '(1 1 1 1 1))
	 1)
        ))))

(defun hash-to-list (hash-table)
  (let ((xx nil))
    (maphash
     (lambda (k v)
       (push (list k v) xx))
     hash-table)
    xx))

(defun compare-cards (c1 c2)
  (let ((r1 (gethash c1 card-ranks))
	(r2 (gethash c2 card-ranks)))
    (cond
      ((> r1 r2)
       t)
      ((< r1 r2)
       nil))))

(defun get-conversion (card)
  (cond
    ((string= "JJJJJ" card)
     "J")
    ((string-match-p "J" card)
       (let* ((counts (count-chars card))
              (ls (hash-to-list counts))
              (wo-j (seq-filter (lambda (el) (not (string= (car el) "J"))) ls))
              (unq (seq-uniq (mapcar (lambda (el) (nth 1 el)) wo-j)))
              (sorted (seq-sort-by (lambda (el) (nth 1 el)) #'> wo-j)))
         (if (equal unq '(1))
           (car (sort (mapcar (lambda (el) (nth 0 el)) wo-j) #'compare-cards))
           (car (car sorted)))))
    (t
      nil)))

(defun convert (card)
  (let ((chr (get-conversion card)))
    (if chr
      (string-replace chr "J" card)
      card)))

(defun compare-card-sets (card-ranks fn c1 c2)
  (let* ((card1 (car c1))
	 (card2 (car c2))
	 (cc1 (funcall fn card1))
	 (cc2 (funcall fn card2))
	 (ct1 (get-type (count-chars cc1)))
 	 (ct2 (get-type (count-chars cc2)))
 	 (s1 (cdr (butlast (split-string (car c1) ""))))
 	 (s2 (cdr (butlast (split-string (car c2) "")))))
    (cond
      ((= ct1 ct2)
       (catch 'break
         (dotimes (i (length s1))
	   (let ((r1 (gethash (nth i s1) card-ranks))
		 (r2 (gethash (nth i s2) card-ranks)))
	     (cond
	       ((> r1 r2)
		(throw 'break t))
	       ((< r1 r2)
		(throw 'break nil))))) nil))
      ((> ct1 ct2)
       t)
      ((< ct1 ct2)
       nil))))

(setq card-ranks1
      #s(hash-table test equal data ("A" 13 "K" 12 "Q" 11 "J" 10 "T" 9 "9" 8 "8" 7 "7" 6 "6" 5 "5" 4 "4" 3 "3" 2 "2" 1)))

(setq card-ranks2
      #s(hash-table test equal data ("A" 13 "K" 12 "Q" 11 "J" 1 "T" 10 "9" 9 "8" 8 "7" 7 "6" 6 "5" 5 "4" 4 "3" 3 "2" 2)))

(defalias 'compare-card-sets1 (apply-partially #'compare-card-sets card-ranks1 'identity))
(defalias 'compare-card-sets2 (apply-partially #'compare-card-sets card-ranks2 'convert))

(defun solve (input fn)
  (setq sorted (nreverse (sort (get-cards-and-bid input) fn)))
  (seq-reduce #'+ (seq-mapn #'* (number-sequence 1 (length sorted)) (mapcar (lambda (el) (string-to-number (nth 1 el))) sorted)) 0))

(message "%s" (solve sample-input #'compare-card-sets1)) ; 6440
(message "%s" (solve input #'compare-card-sets1)) ; 250957639

(message "%s" (solve sample-input #'compare-card-sets2)) ; 5905
(message "%s" (solve input #'compare-card-sets2)) ; 251515496
