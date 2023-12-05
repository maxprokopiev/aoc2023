(require 'seq)

(defun read-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(setq sample-input (butlast (split-string (read-file "sample.txt") "\n")))
(setq input (butlast (split-string (read-file "input.txt") "\n")))

(defun get-seeds (input)
  (let ((seeds-line (nth 0 input)))
    (when (string-match "seeds: \\(.+\\)" seeds-line)
      (mapcar #'string-to-number (split-string (match-string 1 seeds-line) " ")))))

(defun get-seeds2 (input)
  (seq-partition (get-seeds input) 2))

(defun get-map (name input)
  (let ((i 0)
	(found-map nil)
	(raw-map '()))
    (catch 'break
      (while t
        (let ((line (nth i input)))
          (cond
            ((and (not found-map) (string-prefix-p name line))
              (setq found-map t))
            ((and found-map (> 1 (length line)))
              (throw 'break raw-map))
            (found-map
    	  (push line raw-map)))
          (setq i (+ 1 i)))))
    (mapcar (lambda (line) (mapcar #'string-to-number (split-string line " "))) (nreverse raw-map))))

;(setq seed-to-soil (get-map "seed-to-soil" sample-input))
;(setq soil-to-fertilizer (get-map "soil-to-fertilizer" sample-input))
;(setq fertilizer-to-water (get-map "fertilizer-to-water" sample-input))
;(setq water-to-light (get-map "water-to-light" sample-input))
;(setq light-to-temperature (get-map "light-to-temperature" sample-input))
;(setq temperature-to-humidity (get-map "temperature-to-humidity" sample-input))
;(setq humidity-to-location (get-map "humidity-to-location" sample-input))

(setq seed-to-soil (get-map "seed-to-soil" input))
(setq soil-to-fertilizer (get-map "soil-to-fertilizer" input))
(setq fertilizer-to-water (get-map "fertilizer-to-water" input))
(setq water-to-light (get-map "water-to-light" input))
(setq light-to-temperature (get-map "light-to-temperature" input))
(setq temperature-to-humidity (get-map "temperature-to-humidity" input))
(setq humidity-to-location (get-map "humidity-to-location" input))

(defun get-mapping (seed mp)
  (remove nil (mapcar (lambda (triplet)
			(let ((s1 (nth 0 triplet))
			      (s2 (nth 1 triplet))
			      (amount (nth 2 triplet)))
			  (if (and (>= seed s2) (<= seed (+ s2 (- amount 1))))
			    (+ s1 (- seed s2))
			    nil))) mp)))

(defun get-reverse-mapping (location mp)
  (remove nil (mapcar (lambda (triplet)
			(let ((s2 (nth 0 triplet))
			      (s1 (nth 1 triplet))
			      (amount (nth 2 triplet)))
			  (if (and (>= location s2) (<= location (+ s2 (- amount 1))))
			    (+ s1 (- location s2))
			    nil))) mp)))

(defun m2mr (location mp)
  (let ((r (get-reverse-mapping location mp)))
    (if (= 1 (length r))
      (car r)
      location)))

(defun s2sr (seed)
  (m2mr seed seed-to-soil))

(defun s2fr (seed)
  (m2mr seed soil-to-fertilizer))

(defun f2wr (seed)
  (m2mr seed fertilizer-to-water))

(defun w2lr (seed)
  (m2mr seed water-to-light))

(defun l2tr (seed)
  (m2mr seed light-to-temperature))

(defun t2hr (seed)
  (m2mr seed temperature-to-humidity))

(defun h2lr (seed)
  (m2mr seed humidity-to-location))

(defun m2m (seed mp)
  (let ((r (get-mapping seed mp)))
    (if (= 1 (length r))
      (car r)
      seed)))

(defun s2s (seed)
  (m2m seed seed-to-soil))

(defun s2f (seed)
  (m2m seed soil-to-fertilizer))

(defun f2w (seed)
  (m2m seed fertilizer-to-water))

(defun w2l (seed)
  (m2m seed water-to-light))

(defun l2t (seed)
  (m2m seed light-to-temperature))

(defun t2h (seed)
  (m2m seed temperature-to-humidity))

(defun h2l (seed)
  (m2m seed humidity-to-location))

(defun get-location (seed input)
  (h2l (t2h (l2t (w2l (f2w (s2f (s2s seed))))))))

(defun get-seed (location)
  (s2sr (s2fr (f2wr (w2lr (l2tr (t2hr (h2lr location))))))))

(defun is-in-seeds (num seed-pairs)
  (seq-find (lambda (el)
	      (and (>= num (car el)) (<= num (+ (car el) (- (nth 1 el) 1))))) seed-pairs))

(defun solve1 (input)
  (apply #'min (mapcar (lambda (seed) (get-location seed input)) (get-seeds input))))

(defun solve2 (input)
  (let ((seed-pairs (get-seeds2 input))
	(location 0))
    (catch 'break
      (while t
	     (let ((seed (get-seed location)))
	       (when (is-in-seeds seed seed-pairs)
		 (throw 'break location)))
	     (setq location (+ location 1))))))

;(message "%s" (solve1 sample-input)) ; 35
;(message "%s" (solve1 input)) ; 278755257

;(message "%s" (solve2 sample-input)) ; 46
(message "%s" (solve2 input)) ; 26829166

