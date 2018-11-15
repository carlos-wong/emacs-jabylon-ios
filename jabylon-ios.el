(defun carlos/split-string (string &optional separators omit-nulls keep-sep)
  "Split STRING into substrings bounded by matches for SEPARATORS."
  (let* ((keep-nulls (not (if separators omit-nulls t)))
         (rexp (or separators split-string-default-separators))
         (start 0)
         this-start this-end
         notfirst
         (list nil)
         (push-one
          (lambda ()
            (when (or keep-nulls (< this-start this-end))
              (let ((this (substring string this-start this-end)))
                (when (or keep-nulls (> (length this) 0))
                  (push this list)))))))
    (while (and (string-match
                 rexp string
                 (if (and notfirst
                          (= start (match-beginning 0))
                          (< start (length string)))
                     (1+ start) start))
                (< start (length string)))
      (setq notfirst t)
      (setq this-start start this-end (match-beginning 0)
            start (match-end 0))
      (funcall push-one)
      (when keep-sep
        (push (match-string 0 string) list)))
    (setq this-start start this-end (length string))
    (funcall push-one)
    (nreverse list)))

(defun carlos/jayblon/convertCodePoint (string)
  "docstring"
  (reduce (lambda (init value )
            (+ (string-to-number value 16) (lsh init 4))
            )
          (delq 0 (mapcar (lambda (char)
                            (if (or (equal char 92) (equal char 117))
                                0
                              (format "%c" char)))
                          string)) :initial-value 0))

(provide 'jabylon-ios)
