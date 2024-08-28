;;;-*-EMACS-LISP-*-

;;; llm.el - LLM-based rewriting and summarization functions for Emacs buffers
;;;
;;; Copyright (C) 2024 Leigh Klotz <klotz@klotz.me>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;;
;;; Summary:
;;; This package provides functions for summarizing and rewriting the contents
;;; of Emacs buffers using an external script called 'llm.sh'. The package
;;; includes the following functions:
;;;   - M-X llm-summarize-buffer
;;;     Summarizes the contents of the current bufffer in a new buffer.
;;;   - M-X llm-rewrite
;;;     Replaces region with results of prompt run on region
;;;   - M-X llm-ask
;;;     Answers question about region in a new buffer
;;;   - M-X llm-write
;;;     Writes a response based on the region and prompt in a new buffer
;;;
;;; Dependencies:
;;;   - A shell script called 'llm-emacs-helper.sh' that contains the command(s) you want
;;;     to run on the buffer or region content.

(defcustom llm-rewrite-script-path
  "~/wip/llamafiles/llm_el/llm-emacs-helper.sh"
  "Path to the LLM rewrite script."
  :type 'string
  :group 'llm)

(defcustom llm-default-via
  "api"
  "Default VIA for LLM."
  :type '(choice
	  (const api)
	  (const cli))
  :group 'llm)

(defcustom llm-default-model-type
  ;; one of: cerebrum codebooga deepseek-coder dolphin functions.sh llava mistral mixtral models.jsonl nous-hermes phi rocket 
  "mistral" 
  "Default model type for LLM."
  :type '(choice
          (const cerebrum)
          (const codebooga)
          (const deepseek-coder)
          (const dolphin)
          (const mistral)
          (const mixtral)
          (const nous-hermes)
          (const phi)
          (const rocket))
  :group 'llm)

(defvar llm-ask-buffer-name     "*llm-ask*")
(defvar llm-write-buffer-name   "*llm-write*")
(defvar llm-summary-buffer-name "*llm-summary*")
(defvar llm-error-buffer-name   "*llm-errors*")

;;; User commands
(defun llm-ask (prompt start end)
  "Writes a new buffer based on the prompt and current region, and the output of the llm-rewrite-script-path command"
  (interactive "sQuestion: \nr")
  (llm-region-internal "ask" llm-default-via llm-default-model-type (llm-mode-text-type) prompt start end llm-ask-buffer-name nil))

(defun llm-summarize-buffer (user-prompt)
  "Creates a new buffer containing a summary of the current buffer, with a user prompt."
  (interactive "sSummarize Buffer Prompt: \n")
  (llm-region-internal "summarize" llm-default-via llm-default-model-type (llm-mode-text-type) user-prompt (point-min) (point-max) llm-summary-buffer-name nil))

(defun llm-insert (prompt start end)
  "Insert inferred text based on the prompt and current region in the current buffer."
  (interactive "sPrompt: \nr")
  (let ((llm-write-buffer-name  t))	;insert into current buffer
    (llm-region-internal "write" llm-default-via llm-default-model-type (llm-mode-text-type) prompt start end llm-write-buffer-name nil)))

(defun llm-complete (prompt start end)
  "Insert some inferred text based on current region to point in the current buffer. "
  ;; todo: ignores end and uses (point); should have good default behavior and get bounds better.
  (interactive "sPrompt: \nr")
  (let ((n-predict 32))			;fixme ctrl-u arg?
    (llm-complete-internal prompt llm-default-via llm-default-model-type start end n-predict)))

(defun llm-write (prompt start end)
  "Writes a new buffer based on the prompt and current region, and the output of the llm-rewrite-script-path command"
  (interactive "sPrompt: \nr")
  (llm-region-internal "write" llm-default-via llm-default-model-type (llm-mode-text-type) prompt start end llm-write-buffer-name nil))

(defun llm-rewrite (user-prompt start end)
  "Rewrites the current region with the output of the llm-rewrite-script-path command based on the prompt and current region"
  (interactive "sRewrite Prompt: \nr")
  (llm-region-internal "rewrite" llm-default-via llm-default-model-type (llm-mode-text-type) user-prompt start end nil t))

(defun llm-todo (user-prompt start end)
  "Rewrites the current region to process 'todo' items with the output of the llm-rewrite-script-path command based on the prompt and current region"
  (interactive "sRewrite Prompt: \nr")
  (llm-region-internal "todo" llm-default-via llm-default-model-type (llm-mode-text-type) user-prompt start end nil t))

;; This function is used in comint-mode to understand and explain the output in an
;; interactive way. It prompts the user with a default question, "What line
;; number contains the proximal error?", or a custom prompt if provided. It
;; then uses the output between the last two output boundaries to generate an
;; explanation through the llm-ask function.
(defun llm-explain-output (prompt)
  (interactive "sQuestion: ")
  (let* ((bounds (my-comint-get-previous-output-bounds))
	 (start (car bounds))
	 (end (cadr bounds))
	 (default-prompt "What line number contains the proximal error?")
	 (prompt (if (or (null prompt) (string= "" prompt))
		     default-prompt
		   prompt)))
    (llm-ask prompt start end)))

;; This function is used in comint-mode to understand and explain the output in an
;; interactive way. It prompts the user with a default question, "What line
;; number contains the proximal error?", or a custom prompt if provided. It
;; then uses the output between the last two output boundaries to generate an
;; explanation through the llm-ask function.
(defun llm-explain-output (prompt)
  (interactive "sQuestion: ")
  (let* ((bounds (my-comint-get-previous-output-bounds))
	 (start (car bounds))
	 (end (cadr bounds))
	 (default-prompt "What line number contains the proximal error?")
	 (prompt (if (or (null prompt) (string= "" prompt))
		     default-prompt
		   prompt)))
    (llm-ask prompt start end)))

(defun llm-infer-command-internal (&rest args)
  (concat
   llm-rewrite-script-path
   " "
   (mapconcat #'(lambda (arg) (shell-quote-argument (format "%s" arg))) args " ")))

;;; Interface to rewrite.sh
(defun llm-region-internal (use-case via model-type major-mode-name user-prompt start end output-buffer-name replace-p)
  "Send the buffer or current region as the output of the llm-rewrite-script-path command based on the prompt and current region and either replaces the region or uses a specified buffer, based on output-buffer-name and replace-p.
See [shell-command-on-region] for interpretation of output-buffer-name."
  ;; Send the buffer or selected region as a CLI input to 'llm.sh'
  (let ((start (or start (point-min)))
	(end (or end (point-max)))
	(command (llm-infer-command-internal use-case model-type via major-mode-name user-prompt))
	(display-error-buffer t)
	(region-noncontiguous-p nil))
    ;; many args, make sure to call properly
    (message "llm-region-internal: buffer=%s[%s,%s] command=%s replace-p=%s output-buffer-name=%s" (buffer-name) start end command replace-p output-buffer-name)
    (let ((max-mini-window-height 0.0))
      (shell-command-on-region start end command output-buffer-name replace-p llm-error-buffer-name display-error-buffer region-noncontiguous-p))))


(defun llm-complete-internal (prompt via model-type start end n-predict)
  ;; Send the buffer or selected region as a CLI input to 'llm.sh'
  (let* ((command (llm-infer-command-internal "complete" model-type via (llm-mode-text-type) n-predict prompt))
	 (display-error-buffer t)
	 (region-noncontiguous-p nil))
    (message "llm-commplete-internal: %s" command)
    (let ((max-mini-window-height 0.0)
	  (old-text (buffer-substring start end))
	  (new-end))
      ;; todo: better prompting, put point at end, leave in place for repeated application, temperature, etc
      (shell-command-on-region start end command nil t llm-error-buffer-name display-error-buffer region-noncontiguous-p)
      (goto-char start)
      (insert old-text))))

(defun llm-mode-text-type ()
  (symbol-name major-mode))

;;; these probably belong elsewhere
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


;;;
;;; my keybindings, should move out
;;; 

(global-set-key (kbd "M-s $") 'llm-summarize-buffer)
(global-set-key (kbd "M-s r") 'llm-rewrite)
(global-set-key (kbd "M-s x") 'llm-explain-output)
(global-set-key (kbd "M-s i") 'llm-insert)
(global-set-key (kbd "M-s c") 'llm-complete)


(provide 'llm)
