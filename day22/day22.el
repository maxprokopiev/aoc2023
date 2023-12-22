(require 'seq)
(require 'subr-x)

(defun read-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun remove-nth-element (nth list)
  (if (zerop nth) (cdr list)
    (let ((last (nthcdr (1- nth) list)))
      (setcdr last (cddr last))
      list)))

(defun parse-input (file)
  (mapcar (lambda (line)
	    (mapcar (lambda (c)
		      (mapcar 'string-to-number (split-string c "," t)))
		    (split-string line "~")))
	  (split-string (read-file file) "\n" t)))

(defun sort-input (input)
  (seq-sort-by (lambda (pair) (min (nth 2 (nth 0 pair)) (nth 2 (nth 1 pair)))) #'< input))

(defun intersect (ax ay bx by cx cy dx dy)
  (setq line1 nil)
  (dolist (xs (number-sequence ax bx))
    (dolist (ys (number-sequence ay by))
      (push (list xs ys) line1)))

  (setq line2 nil)
  (dolist (xs (number-sequence cx dx))
    (dolist (ys (number-sequence cy dy))
      (push (list xs ys) line2)))

  (seq-intersection line1 line2))

(defun does-collide (brick bricks)
  (seq-let (c1 c2) brick
    (seq-let (x1 y1 z1) c1
      (seq-let (x2 y2 z2) c2
        (seq-some (lambda (b)
          (seq-let (cb1 cb2) b
            (seq-let (xb1 yb1 zb1) cb1
              (seq-let (xb2 yb2 zb2) cb2
		(and
		  (<= (min z1 z2) (max zb1 zb2))
                  (intersect x1 y1 x2 y2 xb1 yb1 xb2 yb2)))))) bricks)))))

(defun drop (brick ground)
  (setq z (min (nth 2 (nth 0 brick)) (nth 2 (nth 1 brick))))
  (if (= z 1)
    brick
    (progn
      (setq to-check-with (seq-filter (lambda (pair)
            				    (let* ((c1 (nth 0 pair))
            					   (c2 (nth 1 pair))
            					   (z1 (nth 2 c1))
            					   (z2 (nth 2 c2)))
            				      (and (<= (min z1 z2) (- z 1)) (>= (max z1 z2) (- z 1))))) ground))
      (seq-let (c1 c2) brick
        (seq-let (x1 y1 z1) c1
          (seq-let (x2 y2 z2) c2
            (setq new-brick (list (list x1 y1 (- z1 1)) (list x2 y2 (- z2 1)))))))

      (if (does-collide new-brick to-check-with)
        brick
        (drop new-brick ground)))))

(defun solve (file)
  (setq bricks (sort-input (parse-input file)))
  (setq ground nil)

  (dolist (brick bricks)
    (setq ground (append ground (list (drop brick ground)))))

  (defun remove-and-drop (ind ground)
    (setq c 0)
    (setq bricks (copy-sequence ground))
    (setq bricks (remove-nth-element ind bricks))
    (setq new-ground nil)

    (dolist (brick bricks)
      (setq new-brick (drop brick new-ground))
      (when (not (equal brick new-brick))
        (setq c (+ c 1)))
      (setq new-ground (append new-ground (list new-brick))))
    (list c (equal bricks new-ground)))

  (setq part1 0)
  (setq part2 0)
  (dotimes (i (length ground))
    (seq-let (n didnt-drop) (remove-and-drop i ground)
      (if didnt-drop
        (setq part1 (+ part1 1))
        (setq part2 (+ part2 n)))))
  (list part1 part2))

(message "%s" (solve "sample.txt")) ; 5  7
(message "%s" (solve "input.txt")) ; 448 | 57770

