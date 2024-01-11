(defun find-odd-quotes ()
  "Go to the end of the next line in the buffer that has an odd number of quote characters."
  (interactive)
  (while (and (not (eobp))
              (zerop (% (count-matches "\"" (point) (line-end-position)) 2)))
    (forward-line 1))
  (if (not (eobp))
      (goto-char (line-end-position))
    (message "No line with odd number of quote characters found")))


(defun find-odd-apostropnes ()
  "Go to the end of the next line in the buffer that has an odd number of apostrophe characters."
  (interactive)
  (while (and (not (eobp))
              (zerop (% (count-matches "'" (point) (line-end-position)) 2)))
    (forward-line 1))
  (if (not (eobp))
      (goto-char (line-end-position))
    (message "No line with odd number of apostrophe characters found")))
