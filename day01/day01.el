(require 'seq)

(defun read-file (filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (buffer-string)))

(defun firststr (str)
  (substring str 0 1))

(defun laststr (str)
  (substring str (- (length str) 1) (length str)))

(defun remove-non-digits (str)
  (replace-regexp-in-string "[^0-9]" "" str))

(defun getnum (str)
  (let ((digits (remove-non-digits str)))
    (string-to-number (concat (firststr digits) (laststr digits)))))

(defun solve1 (input)
  (seq-reduce #'+
    (mapcar #'getnum
	input) 0))

(message (number-to-string (solve1 (butlast (split-string (read-file "sample.txt") "\n"))))) ; 142
(message (number-to-string (solve1 (butlast (split-string (read-file "input.txt") "\n"))))) ; 54940

(defun getfirst (str)
  (let ((digits (remove-non-digits str)))
    (firststr digits)))

(defun getlast (str)
  (let ((digits (remove-non-digits str)))
    (laststr digits)))

(defun rreplace (re with str)
  (shell-command-to-string (format "echo '%s' | ruby -e \"puts gets.chomp.gsub(/%s/, '%s')\"" str re with)))

(defun replace-with-lookbehind (str)
  (seq-reduce (lambda (acc pair) (rreplace (car pair) (cdr pair) acc))
    '(("(?<!tw)one" . "1") ("(?<!eigh)two" . "2") ("(?<!eigh)three" . "3") ("four" . "4") ("five" . "5") ("six" . "6") ("seven" . "7") ("((?<!on)|(?<!thre)|(?<!fiv)|(?<!nin))eight" . "8") ("(?<!seve)nine" . "9")) str))

(defun replace-with-lookahead (str)
  (seq-reduce (lambda (acc pair) (rreplace (car pair) (cdr pair) acc))
    '(("one(?!ight)" . "1") ("two(?!ne)" . "2") ("three(?!ight)" . "3") ("four" . "4") ("five(?!ight)" . "5") ("six" . "6") ("seven(?!ine)" . "7") ("eight((?!wo)|(?!hree))" . "8") ("nine(?!ight)" . "9")) str))

(defun solve2(input)
  (seq-reduce #'+
    (mapcar (lambda (str) (string-to-number (concat (getfirst (replace-with-lookbehind str)) (getlast (replace-with-lookahead str)))))
	input) 0))

(message (number-to-string (solve2 (butlast (split-string (read-file "sample2.txt") "\n"))))) ; 281
(message (number-to-string (solve2 (butlast (split-string (read-file "input.txt") "\n"))))) ; 54208
