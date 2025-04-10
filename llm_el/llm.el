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
;;;     Summarizes the contents of the current buffer in a new buffer.
;;;   - M-X llm-rewrite
;;;     Replaces region with results of prompt run on region
;;;   - M-X llm-ask
;;;     Answers question about region in a new buffer
;;;   - M-X llm-write
;;;     Writes a response based on the region and prompt in a new buffer
;;;   - M-X llm-load-model
;;;     Load the specified user model; offers a completing reader.
;;;
;;; Some commands will use empty string if there is not a region, but other commands will error.
;;;
;;; Dependencies:
;;;   - A shell script called 'llm-emacs-helper.sh' that contains the command(s) you want
;;;     to run on the buffer or region content.

;;; TODO: make all interactive functions that accept prompt use a `llm-prompt-history` ring

(defcustom llm-rewrite-script-path
  "~/wip/llamafiles/llm_el/llm-emacs-helper.sh"
  "Path to the LLM rewrite script."
  :type 'string
  :group 'llm)

(defcustom llm-via-script-path
  "~/wip/llamafiles/scripts/via.sh"
  "Path to the LLM via script."
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
(defvar llm-diff-buffer-name    "*llm-diff*")
(defvar llm-preserve-temp-files nil)
(defvar llm-git-merge-format "git merge-file -p \"%s\" /dev/null \"%s\"")

;;; User commands
(defun llm-ask (prompt &optional start end)
  "Writes a new buffer based on the prompt and current region, and the output of the llm-rewrite-script-path command.
If no region is selected, the function will assume the entire buffer is the region."
  (interactive "sQuestion: \nr")
  ;; Check if the region is active, if not, use empty string
  (unless (and start end)
    (setq start (point-min))
    (setq end (point-max)))
  (llm-region-internal "ask" llm-default-via llm-default-model-type (llm-mode-text-type) prompt start end llm-ask-buffer-name nil nil))

(defun llm-summarize-buffer (user-prompt)
  "Creates a new buffer containing a summary of the current buffer, with a user prompt."
  (interactive "sSummarize Buffer Prompt: \n")
  (llm-region-internal "summarize" llm-default-via llm-default-model-type (llm-mode-text-type) user-prompt (point-min) (point-max) llm-summary-buffer-name nil nil))

(defun llm-insert (prompt &optional start end)
  "Insert inferred text based on the prompt and current region in the current buffer."
  (interactive "sPrompt: \nr")
  ;; Check if the region is active, if not, use empty string
  (unless (and start end)
    (setq start (point-min))
    (setq end (point-max)))
  (let ((llm-write-buffer-name t))	;insert into current buffer
    (llm-region-internal "write" llm-default-via llm-default-model-type (llm-mode-text-type) prompt start end llm-write-buffer-name nil nil)))

(defun llm-complete (prompt start end)
  "Insert some inferred text based on current region to point in the current buffer."
  ;; todo: ignores end and uses (point); should have good default behavior and get bounds better.
  (interactive "sPrompt: \nr")
  (let ((n-predict 32))			;fixme ctrl-u arg?
    (llm-complete-internal prompt llm-default-via llm-default-model-type start end n-predict)))

(defun llm-write (prompt &optional start end)
  "Writes a new buffer based on the prompt and current region, and the output of the llm-rewrite-script-path command"
  (interactive "sPrompt: \nr")
  ;; Check if the region is active, if not, use empty string
  (unless (and start end)
    (setq start (point-min))
    (setq end (point-max)))
  (llm-region-internal "write" llm-default-via llm-default-model-type (llm-mode-text-type) prompt start end llm-write-buffer-name nil nil))

(defun llm-rewrite (user-prompt start end)
  "Rewrites the current region with the output of the llm-rewrite-script-path command based on the prompt and current region"
  (interactive "sRewrite Prompt: \nr")
  (llm-region-internal "rewrite" llm-default-via llm-default-model-type (llm-mode-text-type) user-prompt start end nil t t))

(defun llm-todo (user-prompt start end)
  "Rewrites the current region to process 'todo' items with the output of the llm-rewrite-script-path command based on the prompt and current region"
  (interactive "sRewrite Prompt: \nr")
  (llm-region-internal "todo" llm-default-via llm-default-model-type (llm-mode-text-type) user-prompt start end nil t t))

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
(defun llm-region-internal (use-case via model-type major-mode-name user-prompt start end output-buffer-name replace-p diff-p)
  "Send the buffer or current region as the output of the llm-rewrite-script-path command based on the prompt and current region and either replaces the region or uses a specified buffer, based on output-buffer-name and replace-p.
See [shell-command-on-region] for interpretation of output-buffer-name."
  ;; Send the buffer or selected region as a CLI input to 'llm.sh'
  (let ((start (or start (point-min)))
	(end (or end (point-max)))
	(command (llm-infer-command-internal use-case via model-type major-mode-name user-prompt))
	(display-error-buffer t)
	(region-noncontiguous-p nil))
    (message "llm-region-internal: buffer=%s[%s,%s] command=%s replace-p=%s diff-p=%s output-buffer-name=%s" (buffer-name) start end command replace-p diff-p output-buffer-name)
    (let ((max-mini-window-height 0.0))
      (if (and replace-p diff-p)
          (llm-region-as-diff-internal start end command)
        (let ((output-buffer (get-buffer-create output-buffer-name)))
          (if (and replace-p (buffer-file-name) (equal (buffer-file-name) (buffer-file-name output-buffer)))
              (shell-command-on-region start end command nil t llm-error-buffer-name display-error-buffer region-noncontiguous-p)
            (shell-command-on-region start end command output-buffer-name replace-p llm-error-buffer-name display-error-buffer region-noncontiguous-p)
            (pop-to-buffer output-buffer)))))))

(defun llm-region-as-diff-internal (start end command)
  (let* ((original-string (buffer-substring start end))
         (llm-output (with-temp-buffer
                       (insert original-string)
                       (shell-command-on-region (point-min) (point-max) command (current-buffer) t llm-error-buffer-name t nil)
                       (buffer-string))))
    (llm-diff-current-buffer-with-string llm-output)))

;;; when replacing, insert diffs in conflict merge marker format
;;; Background:
;;; Emacs automatically highlights merge conflict markers (like <<<<<<<, =======, >>>>>>>) in files with conflicts, allowing you to easily identify and resolve them. You can trigger this by opening a file with merge conflict markers in Emacs, and the diff display will automatically activate.
;;; smerge-mode offers convenient commands for this format
(defun llm-diff-current-buffer-with-string (new-string)
  "Handy \\[smerge] between the current buffer content and new-string"
  (interactive "sEnter the new string: ")
  (let* ((current-buffer-content (buffer-substring-no-properties (point-min) (point-max)))
         (temp-file-before (make-temp-file "emacs-diff-before-"))
         (temp-file-after (make-temp-file "emacs-diff-after-"))
         (diff-buffer (get-buffer-create llm-diff-buffer-name))
         ;; git merge-file -p "$before" "$after" /dev/null > "$output"
         (diff-command (format llm-git-merge-format temp-file-before temp-file-after)))
    (message "diff-command %s" diff-command)

    (with-temp-file temp-file-before
      (insert current-buffer-content))

    (with-temp-file temp-file-after
      (insert new-string))

    ;; Run the diff command and capture the output in the *llm-diff* buffer
    (let ((result (shell-command-to-string diff-command)))
      (erase-buffer)
      (insert result)
      (smerge-mode)
      (diff-auto-refine-mode 1)
      (goto-char (point-min)))

    (unless llm-preserve-temp-files
      (delete-file temp-file-before)
      (delete-file temp-file-after))))

(defun llm-complete-internal (prompt via model-type start end n-predict)
  ;; Send the buffer or selected region as a CLI input to 'llm.sh'
  (let* ((command (llm-infer-command-internal "complete" via model-type (llm-mode-text-type) n-predict prompt))
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

(defun llm-load-model ()
  "Load the specified model using LLM script."
  (interactive)
  (let* ((model-names (with-temp-buffer
			(when (call-process llm-via-script-path nil t nil "--api" "--list-models")
			  (split-string (buffer-string) "\n" t))))
         (model-name (completing-read "Model: " model-names)))
    (when model-name
      (let ((results
	     (with-temp-buffer
	       (call-process llm-via-script-path nil t nil "--api" "--load-model" model-name)
	       (split-string (buffer-string) "\n" t))))
	(message "model-name=%s results=%s" model-name results)))))

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

;; todo: not working
(defun llm-query-replace (regex prompt)
  "Query replace occurrences matching REGEX with text generated by LLM using PROMPT.

This function is similar to `query-replace-regexp`, but instead of replacing
with a fixed string, it uses an LLM to generate the replacement text based on
the provided prompt and the matched text.

Parameters:
- REGEX: A regular expression string to match text in the buffer.
- PROMPT: A string that serves as a prompt for the LLM. It should contain
          a `%s` placeholder, which will be replaced by the matched text.

Usage:
1. Call the function with M-x llm-query-replace.
2. Enter the regular expression when prompted.
3. Enter the LLM prompt, ensuring it includes `%s` where the matched text will be inserted.
4. The function will find all occurrences of the regex in the buffer.
5. For each match, it will ask if you want to replace it with LLM-generated text.
6. If you agree, it will generate the replacement text using the LLM and replace the match.

Note:
- Ensure that the LLM function `llm-function` is already implemented and available.
- The replacement is undoable using the standard undo functionality in Emacs."
  (interactive "sRegex: \nsLLM Prompt: ")
  (let ((replacer (lambda (match)
                    (let* ((llm-input (format prompt match))
                           (llm-output (llm-function llm-input))) ; Assume llm-function is already defined
                      llm-output))))
    (query-replace-regexp regex replacer)))

(defun llm-quick (question)
  "Calls \\[llm-insert] with question and an exhortation to be succinct."
  (interactive "sQuestion: ")
  (llm-insert (format "briefly write %s and nothing else" question)))

;;;
;;; emacs-lisp reflective help
;;;
(defun llm-apropos (apropos-match question)
  "Calls \\[apropos] with apropos-match and then \\[llm-ask] with question"
  (interactive "sApropos: \nsQuestion: ")
  (save-excursion
    (apropos apropos-match)
    (llm-ask question (point-min) (point-max))))

;;; Get function documentation and ask a question about it
;;; Not sure
(defun llm-describe-function (function-name question)
  "Calls \\[documentation] with function-name and then \\[llm-ask]] with question.
Function-name is completed from the list of defined Emacs Lisp functions."
  (interactive
   (list (intern (completing-read "Function: " obarray 'fboundp))
         (read-string "Question: ")))
  (save-excursion
    (describe-function function-name)
    (set-buffer (help-buffer))
    (llm-ask question (point-min) (point-max))))

(defun llm-describe-variable (variable-name question)
  "Calls `describe-variable` with variable-name and then `llm-ask` with question.
Variable-name is completed from the list of defined Emacs Lisp variables."
  (interactive 
   (list (intern (completing-read "Variable: " obarray 'boundp))
         (read-string "Question: ")))
  (save-excursion
    (describe-variable variable-name)
    (set-buffer (help-buffer))
    (llm-ask question (point-min) (point-max))))



;;;
;;; my keybindings, should move out
;;; 

(global-set-key (kbd "M-s a") 'llm-ask)
(global-set-key (kbd "M-s $") 'llm-summarize-buffer)
(global-set-key (kbd "M-s r") 'llm-rewrite)
(global-set-key (kbd "M-s x") 'llm-explain-output)
(global-set-key (kbd "M-s t") 'llm-todo)
(global-set-key (kbd "M-s i") 'llm-insert)
(global-set-key (kbd "M-s c") 'llm-complete)
(global-set-key (kbd "C-S-n") 'smerge-next)
(global-set-key (kbd "C-S-p") 'smerge-prev)

;;; Hooks
(defun llm-smerge-mode-hook ()
  (local-set-key (kbd "C-S-n") 'smerge-next)
  (local-set-key (kbd "C-S-p") 'smerge-prev)
  (local-set-key (kbd "C-S-u") 'smerge-keep-upper)
  (local-set-key (kbd "C-S-l") 'smerge-keep-lower))
(add-hook 'smerge-mode-hook 'llm-smerge-mode-hook)

(provide 'llm)

