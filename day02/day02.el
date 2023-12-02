(defun read-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(setq sample-input (butlast (split-string (read-file "sample.txt") "\n")))
(setq input (butlast (split-string (read-file "input.txt") "\n")))

(defun get-game-id (s)
    (save-match-data
	(and (string-match "Game \\([0-9]+\\): " s)
	    (string-to-number (match-string 1 s)))))

(defun get-game-sets (s)
    (save-match-data
	(and (string-match "Game \\([0-9]+\\): \\(.*\\)" s)
	    (split-string (match-string 2 s) "; "))))

(defun is-possible (sets red blue green)
  (seq-every-p (lambda (set)
		 (let ((cubes (split-string set ", ")))
		   (seq-every-p (lambda (cube)
				  (let ((pair (split-string cube " ")))
				    (cond
				     ((string= "red" (nth 1 pair))
				      (>= red (string-to-number (nth 0 pair))))
				     ((string= "blue" (nth 1 pair))
				      (>= blue (string-to-number (nth 0 pair))))
				     ((string= "green" (nth 1 pair))
				      (>= green (string-to-number (nth 0 pair))))))) cubes))
		 ) sets))

(defun solve1 (input)
  (seq-reduce #'+
    (mapcar (lambda (line)
	    (let ((game-id (get-game-id line))
		    (sets (get-game-sets line)))
		(if (is-possible sets 12 14 13)
		    game-id
		    0))) input) 0))
 
(message "%s" (solve1 sample-input))
(message "%s" (solve1 input)) ; 2283

(defun re-seq (regexp string)
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      matches)))

(defun remove-non-digits (str)
  (replace-regexp-in-string "[^0-9]" "" str))

(defun get-max (s color)
  (apply #'max (mapcar #'string-to-number (mapcar #'remove-non-digits (re-seq (concat "[0-9]+ " color) s)))))

(defun solve2 (input)
  (seq-reduce #'+ (mapcar (lambda (line)
			    (let ((power (* (get-max line "red") (get-max line "green") (get-max line "blue"))))
			      power)) input) 0))

(message "%s" (solve2 sample-input))
(message "%s" (solve2 input)) ; 78669
