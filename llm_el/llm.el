;;;-*-EMACS-LISP-*-

;;; This package provides LLM-based rewriting and summarization in
;;; Emacs buffers using an external script `llm_el/rewrite.sh`.
;;;
;;; The main functions provided are:
;;; 
;;; 1. `llm-ask`: Creates a new buffer containing a response to teh
;;; question, based on the prompt and current region, and the output
;;; of the `llm-rewrite-script-path` command.
;;; 
;;; 2. `llm-summarize-buffer`: Creates a new buffer containing a
;;; summary of the current buffer, with an optional user-provided
;;; prefix argument as a prompt.
;;; 
;;; 3. `llm-rewrite`: Rewrites the current region with the output of
;;; the `llm-rewrite-script-path` command based on the prompt and
;;; current region.
;;;
;;; 4. `llm-region-internal`: Sends the buffer or current region as
;;; the output of the `llm-rewrite-script-path` command based on the
;;; prompt and current region. It can replace the region or use a
;;; specified buffer, depending on the `output-buffer` and `replace-p`
;;; parameters.
;;; 
;;; The package also provides a global keybinding `(kbd "M-s $")` to
;;; invoke the `llm-summarize-buffer` function.

(defvar llm-rewrite-script-path "~/wip/llamafiles/llm_el/rewrite.sh")
(defvar llm-ask-buffer-name   "*llm-ask*")
(defvar llm-rewrite-buffer-name "*llm-rewrite*")
(defvar llm-summary-buffer-name "*llm-summary*")

(defun llm-ask (prompt start end &optional output-buffer-name replace-p)
  "Writes a new buffer based on the prompt and current region, and the output of the llm-rewrite-script-path command"
  (interactive "sRequest: \nr")
  (let ((system-prompt
	 (format "Read this following text from major-mode=%s buffer and respond to this request:"
		 (symbol-name major-mode))))
    (llm-region-internal system-prompt prompt start end (or output-buffer-name llm-ask-buffer-name) replace-p)))

(defun llm-summarize-buffer (&optional user-prompt)
  "Creates a new buffer containing a summary of the current buffer, with prefix arg accepts a prompt."
  (interactive 
   (list (let ((default-prompt (format "Summarize the text:")))
	   (read-string (format "Prompt (%s): " default-prompt)
			nil nil
			(format "%s\n" default-prompt)))))
  (llm-ask user-prompt (point-min) (point-max) llm-summary-buffer-name))


(defun llm-region-internal (system-prompt user-prompt start end output-buffer replace-p)
  "Send the buffer or current region as the output of the llm-rewrite-script-path command based on the prompt and current region and either replaces the region or uses a specified buffer, based on output-buffer and replace-p.
See [shell-command-on-region] for interpretation of output-buffer."
  ;; Send the buffer or selected region as a CLI input to 'llm.sh'
  (message "llm-region-internal output-buffer=%s replace-p=%s" output-buffer replace-p)
  (let ((start (or start (point-min)))
	(end (or end (point-max)))
	(command (format "%s %s %s"
			 llm-rewrite-script-path
			 (shell-quote-argument system-prompt)
			 (shell-quote-argument user-prompt)))
	(error-buffer "*llm-region-internal-errors*")
	(display-error-buffer t)
	(region-noncontiguous-p nil))
    ;; many args, make sure to call properly
    (message "llm-region-internal[%s,%s] command=%s output-buffer=%s replace-p=%s" start end command output-buffer replace-p)
    (let ((max-mini-window-height 0.0))
      (shell-command-on-region start end command output-buffer replace-p error-buffer display-error-buffer region-noncontiguous-p))))

(defun llm-rewrite (prompt start end)
  "Rewrites the current region with the output of the llm-rewrite-script-path command based on the prompt and current region"
  (interactive "sRewrite: \nr")
  (let ((system-prompt
	 (format "Read this following text from major-mode=%s buffer and output a replacement according to this request:"
		 (symbol-name major-mode))))
    (llm-region-internal system-prompt prompt start end nil t)))

;;;
;;; my keybindings, should move out
;;; 
(global-set-key (kbd "M-s $") 'llm-summarize-buffer)


(provide 'llm)


;;; (defun foo (x)
;;;   (interactive "sX: ")
;;;   (if (not (numberp x))
;;;       (error "Invalid input"))
;;; 
;;; ;;; LLM RESULTS FOR emacs-lisp Please fix the Lisp math error in the code below and write out new code:
;;;  Here is the updated code according to user instructions:
;;; ```lisp
;;; (defun foo (x)
;;;   (interactive "sX: ")
;;;   (if (not (numberp x))
;;;       (error "Invalid input"))
;;;   ;; Add Lisp math expression here
;;;   (let ((result (/ 10 x)))
;;;     result))
;;; ```

;;; llm.sh "write an emacs lisp function called M-X llm-rewrite that operates on the region and sends the region as a CLI input to a subshell command called 'llm.sh' which itself prints out results that the emacs-lisp function will replace the region with."
;;; 
;;; Here is an example of an Emacs Lisp function called `M-X llm-rewrite` that operates on the current region and sends it as a CLI input to a subshell command called 'llm.sh':
;;; ```
;;; (defun llm-rewrite ()
;;;   "Rewrites the current region with the output of the 'llm.sh' command"
;;;   (interactive)
;;;   (let ((region (buffer-substring-no-trailing-whitespace)))
;;;     ;; Send the region as a CLI input to 'llm.sh'
;;;     (shell-command "echo '" region "' | sh llm.sh")
;;;     ;; Replace the region with the output of 'llm.sh'
;;;     (replace-buffer-substring region 0 (buffer-length) (shell-command "echo '" region "' | sh llm.sh)))))
;;; ```
;;; This function first defines a local variable `region` that contains the current region. It then sends the region as a CLI input to 'llm.sh' using the `shell-command` function, which runs the command in a subshell and returns its output. Finally, it replaces the current region with the output of 'llm.sh' using the `replace-buffer-substring` function.
;;; 
;;; Note that this is just an example implementation and may need to be modified to fit your specific use case. Additionally, you will need to create a separate shell script called 'llm.sh' that contains the command(s) you want to run on the region.
