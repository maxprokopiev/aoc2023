(require 'seq)
(require 'subr-x)

(defun read-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun parse-workflows (ws)
  (setq h (make-hash-table :test 'equal))
  (let ((wss (split-string (nth 0 spl) "\n" t)))
    (dolist (w wss)
      (string-match "\\(.+\\){\\(.+\\)}" w)
      (let* ((name (match-string 1 w))
             (rules-s (match-string 2 w))
             (rules (split-string rules-s "," t)))
        (puthash name rules h))))
  h)

(defun parse-ratings (r)
  (setq h (make-hash-table :test 'equal))
  (string-match "{\\(.+\\)}" r)
  (let* ((ratings-spl (split-string (match-string 1 r) "," t)))
    (dolist (rating-s ratings-spl)
      (let* ((rating-cat (split-string rating-s "=" t))
	     (cat (nth 0 rating-cat))
	     (rating (string-to-number (nth 1 rating-cat))))
	(puthash cat rating h))))
  h)

(defun parse-input (file)
  (let* ((spl (split-string (read-file file) "\n\n" t))
	 (pws (parse-workflows (nth 0 spl)))
	 (rs (split-string (nth 1 spl) "\n" t))
	 (prs (mapcar 'parse-ratings rs)))
    (list pws prs)))

(defun process-inst (part w)
  (catch 'break
    (dolist (r w)
      (cond
	((string-match ":" r)
	 (if (string-match "<" r)
	   (setq spl "<")
	   (setq spl ">"))
	 (let* ((if-cond (split-string r ":" t))
		(cnd (nth 0 if-cond))
		(res (nth 1 if-cond))
		(cat-num (split-string cnd spl t))
		(cat (gethash (nth 0 cat-num) part))
		(num (string-to-number (nth 1 cat-num))))
	   (cond
	     ((string= "<" spl)
	      (setq eval-res (< cat num)))
	     ((string= ">" spl)
	      (setq eval-res (> cat num))))
	   (when eval-res
	     (throw 'break res))))
	(t
	  (throw 'break r))))))

(defun solve1 (pws prs)
  (setq accepted nil)
  (dolist (part prs)
    (setq res "in")
    (while (and (not (string= res "A")) (not (string= res "R")))
      (let* ((inst (gethash res pws)))
	(setq res (process-inst part inst))
	(when (string= res "A")
	  (push part accepted)))))
  (seq-reduce '+ (mapcar (lambda (part)
	    (+ (gethash "x" part) (gethash "m" part) (gethash "a" part) (gethash "s" part))) accepted) 0))

(message "%s" (apply 'solve1 (parse-input "sample.txt"))) ; 19114
(message "%s" (apply 'solve1 (parse-input "input.txt"))) ; 409898

(defun get-As (pws)
  (let ((paths nil)
	(get-As-- (lambda (path wname pws)
		   (let* ((rules (gethash wname pws)))
		     (dolist (rule rules)
		       (cond
			 ((string-match ":" rule)
			  (let* ((spl (split-string rule ":" t))
				 (cnd (nth 0 spl))
				 (new-w (nth 1 spl)))
			    (cond
			      ((string= new-w "A")
			       (push (append path (list cnd)) paths))
			      ((string= new-w "R"))
			      (t
				(funcall get-As-- (append path (list cnd)) new-w pws)))
			    (cond
			      ((string-match "<" cnd)
			       (let* ((cnd-parts (split-string cnd "<"))
				      (cat (nth 0 cnd-parts))
				      (num (string-to-number (nth 1 cnd-parts))))
				 (setq path (append path (list (string-join (list cat ">" (number-to-string (- num 1))) ""))))))
			      ((string-match ">" cnd)
			       (let* ((cnd-parts (split-string cnd ">"))
				      (cat (nth 0 cnd-parts))
				      (num (string-to-number (nth 1 cnd-parts))))
				 (setq path (append path (list (string-join (list cat "<" (number-to-string (+ num 1))) "")))))))))
			 ((string= rule "A")
			  (push path paths))
			 ((string= rule "R"))
			 (t
			   (funcall get-As-- path rule pws))))))))
    (funcall get-As-- '() "in" pws)
    paths))

(defun get-boundary (fn edge sym cat path)
  (apply fn (append (list edge)
		      (mapcar (lambda (rule)
				(string-to-number (nth 1 (split-string rule sym t))))
			      (seq-filter (lambda (rule)
					    (string= (string-join (list cat sym) "") (substring rule 0 2)))
					  path)))))

(defun solve2 (file)
  (seq-reduce '+ (mapcar (lambda (path)
			   (seq-reduce '* (mapcar (lambda (cat)
						    (- (get-boundary 'min 4001 "<" cat path) (get-boundary 'max 0 ">" cat path) 1)
						    ) (list "x" "m" "a" "s")) 1)
			   ) (get-As (nth 0 (parse-input file)))) 0))

(message "%s" (solve2 "sample.txt")) ; 167409079868000
(message "%s" (solve2 "input.txt")) ; 113057405770956
