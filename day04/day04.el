; Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
; Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
; Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
; Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
; Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
; Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11

(defun read-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(setq sample-input (butlast (split-string (read-file "sample.txt") "\n")))
(setq input (butlast (split-string (read-file "input.txt") "\n")))

(defun get-winning-numbers (s)
    (mapcar #'string-to-number (save-match-data
	(and (string-match "Card.+:  ?\\(.*\\) |.*" s)
	     (split-string (match-string 1 s) "  ?")))))

(defun get-elfs-numbers (s)
    (mapcar #'string-to-number (save-match-data
	(and (string-match "Card.+:.*|  ?\\(.*\\)" s)
	     (split-string (match-string 1 s) "  ?")))))

(defun get-card-points (card)
  (let ((l (length (seq-intersection (get-winning-numbers card) (get-elfs-numbers card)))))
    (if (> l 0) (expt 2 (- l 1)) 0)))

(message "%s" (seq-reduce #'+ (mapcar #'get-card-points sample-input) 0)) ; 13
(message "%s" (seq-reduce #'+ (mapcar #'get-card-points input) 0)) ; 20107

(defun get-card-number (s)
    (string-to-number (save-match-data
	(and (string-match "Card ? ? ?\\([0-9]+\\):" s)
	     (match-string 1 s)))))

(defun get-card-copies (card max)
  (let ((card-number (get-card-number card))
	(number-of-copies (length (seq-intersection (get-winning-numbers card) (get-elfs-numbers card))))
	(copies '()))
    (if (> (+ card-number number-of-copies) max)
	(setq number-of-copies (- max card-number)))
    (dotimes (el number-of-copies)
      (push (+ 1 el card-number) copies))
    copies))

(defun build-dict (input)
  (mapcar (lambda (card) (get-card-copies card (length input))) input))

(defun count-cards (card-num dict)
  (let ((card-copies (nth (- card-num 1) dict)))
    (if (= 0 (length card-copies))
      1
      (+ 1 (seq-reduce #'+ (mapcar (lambda (n) (count-cards n dict)) card-copies) 0)))))

(defun solve2 (input)
  (setq dict (build-dict input))
  (setq cards '())
  (dotimes (c (length input))
    (push (+ c 1) cards))
  (setq cards (nreverse cards))

  (seq-reduce #'+ (mapcar (lambda (card) (count-cards card dict)) cards) 0))

(message "%s" (solve2 sample-input)) ; 30
(message "%s" (solve2 input)); 8172507
