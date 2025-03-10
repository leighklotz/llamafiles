(defun my-comint-get-previous-output ()
  "Return the output of the previous shell command in comint mode."
  (interactive)
  (let* ((bounds (my-comint-get-previous-output-bounds))
	 (start (car bounds))
	 (end (cadr bounds)))
    (buffer-substring-no-properties start end)))

(defun my-comint-get-previous-output-bounds ()
  "Return the bounds of output of the previous shell command in comint mode."
  (let ((start (save-excursion
                 (comint-previous-prompt 1)
                 (forward-line 1)
                 (point)))
        (end (point-max)))
    (list start end)))

;; Example usage
(defun my-comint-print-previous-output ()
  "Print the output of the previous shell command in comint mode."
  (interactive)
  (let ((output (my-comint-get-previous-output)))
    (message "Previous command output:\n%s" output)))

;; Bind the function to a key for convenience, e.g., C-c o
(define-key comint-mode-map (kbd "C-c o") 'my-comint-print-previous-output)
