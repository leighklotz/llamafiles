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
;;; of Emacs buffers using an external script called 'llm-emacs-helper.sh'. The package
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

;;; Configuration
(defgroup llm nil
  "Configuration options for the LLM integration." )

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

;;; Variables
(defvar llm-ask-buffer-name     "*llm-ask*"
  "Name of the buffer to display the LLM ask output.")

(defvar llm-write-buffer-name   "*llm-write*"
  "Name of the buffer to display the LLM write output.")

(defvar llm-summary-buffer-name "*llm-summary*"
  "Name of the buffer to display the LLM summary output.")

(defvar llm-error-buffer-name   "*llm-errors*"
  "Name of the buffer to display LLM errors.")

(defvar llm-diff-buffer-name    "*llm-diff*"
  "Name of the buffer to display LLM diffs.")

(defvar llm-preserve-temp-files nil
  "Whether to preserve temporary files created during diffing.")

(defvar llm-git-merge-format "git merge-file -p \"%s\" /dev/null \"%s\""
  "Format string for the git merge command used in diffing.")

;;; User Commands

(defun llm-ask (prompt &optional start end)
  "Writes a new buffer based on the prompt and current region, and the output of the llm-rewrite-script-path command.
If no region is selected, the function will assume the entire buffer is the region."
  (interactive "sQuestion: \nr")
  (let ((start (or start (point-min)))
        (end (or end (point-max))))
    (llm-region-internal "ask" llm-default-via llm-default-model-type (llm-mode-text-type) prompt start end llm-ask-buffer-name nil nil)))

(defun llm-summarize-buffer (user-prompt)
  "Creates a new buffer containing a summary of the current buffer, with a user prompt."
  (interactive "sSummarize Buffer Prompt: \n")
  (llm-region-internal "summarize" llm-default-via llm-default-model-type (llm-mode-text-type) user-prompt (point-min) (point-max) llm-summary-buffer-name nil nil))

(defun llm-insert (prompt &optional start end)
  "Insert inferred text based on the prompt and current region in the current buffer."
  (interactive "sPrompt: \nr")
  (let ((start (or start (point-min)))
        (end (or end (point-max))))
    (let ((llm-write-buffer-name t))      ;insert into current buffer
      (llm-region-internal "write" llm-default-via llm-default-model-type (llm-mode-text-type) prompt start end llm-write-buffer-name nil nil))))

(defun llm-complete (prompt start end)
  "Insert some inferred text based on current region to point in the current buffer."
  ;; todo: ignores end and uses (point); should have good default behavior and get bounds better.
  (interactive "sPrompt: \nr")
  (let ((n-predict 32))                 ;fixme ctrl-u arg?
    (llm-complete-internal prompt llm-default-via llm-default-model-type start end n-predict)))

(defun llm-write (prompt &optional start end)
  "Writes a new buffer based on the prompt and current region, and the output of the llm-rewrite-script-path command"
  (interactive "sPrompt: \nr")
  (let ((start (or start (point-min)))
        (end (or end (point-max))))
    (llm-region-internal "write" llm-default-via llm-default-model-type (llm-mode-text-type) prompt start end llm-write-buffer-name nil nil)))

(defun llm-rewrite (user-prompt start end)
  "Rewrites the current region with the output of the llm-rewrite-script-path command based on the prompt and current region"
  (interactive "sRewrite Prompt: \nr")
  (llm-region-internal "rewrite" llm-default-via llm-default-model-type (llm-mode-text-type) user-prompt start end (current-buffer) t t))

(defun llm-todo (user-prompt start end)
  "Rewrites the current region to process 'todo' items with the output of the llm-rewrite-script-path command based on the prompt and current region"
  (interactive "sTodo Prompt: \nr")
  (llm-region-internal "todo" llm-default-via llm-default-model-type (llm-mode-text-type) user-prompt start end nil t t))

;; This function is used in comint-mode to understand and explain the output in an
;; interactive way. It prompts the user with a default question, "What line
;; number contains the proximal error?", or a custom prompt if provided. It
;; then uses the output between the last two output boundaries to generate an
;; explanation through the llm-ask function.
(defun llm-explain-output (prompt)
  "Explains output from comint-mode using LLM.  Prompts for a question, or uses a default."
  (interactive "sQuestion: ")
  (let* ((bounds (my-comint-get-previous-output-bounds))
         (start (car bounds))
         (end (cadr bounds))
         (default-prompt "What line number contains the proximal error?")
         (prompt (if (or (null prompt) (string= "" prompt))
                     default-prompt
                   prompt)))
    (llm-ask prompt start end)))

;;; Internal Functions

(defun llm-infer-command-internal (&rest args)
  "Constructs the command string for calling the LLM script."
  (concat
   llm-rewrite-script-path
   " "
   (mapconcat #'(lambda (arg) (shell-quote-argument (format "%s" arg))) args " ")))

(defun llm-region-internal (use-case via model-type major-mode-name user-prompt start end output-buffer-name replace-p diff-p)
  "Sends the buffer or current region as input to 'llm.sh'."
  (message "llm-region-internal: buffer=%s[%s,%s] command=%s replace-p=%s diff-p=%s output-buffer-name=%s"
           (buffer-name) start end (llm-infer-command-internal use-case via model-type major-mode-name user-prompt)
           replace-p diff-p output-buffer-name)
  (let ((output-buffer-name (if replace-p (buffer-name (current-buffer)) output-buffer-name)))
    (cond ((and replace-p diff-p) (llm-region-as-diff-internal start end (llm-infer-command-internal use-case via model-type major-mode-name user-prompt)))
          (t (shell-command-on-region start end (llm-infer-command-internal use-case via model-type major-mode-name user-prompt) output-buffer-name replace-p
                                        llm-error-buffer-name t nil)))))

(defun llm-region-as-diff-internal (start end command)
  "Creates a diff between the original region and the LLM-generated text."
  (let* ((original-string (buffer-substring start end))
         (llm-output (with-temp-buffer
                       (insert original-string)
                       (shell-command-on-region (point-min) (point-max) command (current-buffer) t llm-error-buffer-name t nil)
                       (buffer-string))))
    (message "llm-diff-region-with-string %d %d %s" start end llm-output)
    (llm-diff-region-with-string start end llm-output)))

(defun llm-diff-region-with-string (start end new-string)
  "Handy \\[smerge] between the specified region and new-string"
  (let* ((current-buffer-content (buffer-substring-no-properties start end))
         (temp-file-before (make-temp-file "emacs-diff-before-"))
         (temp-file-after (make-temp-file "emacs-diff-after-"))
         (diff-buffer (get-buffer-create llm-diff-buffer-name))
         (diff-command (format llm-git-merge-format temp-file-before temp-file-after)))
    (message "diff-command %s" diff-command)

    (with-temp-file temp-file-before
      (insert current-buffer-content))

    (with-temp-file temp-file-after
      (insert new-string))

    ;; Run the diff command and capture the output in the *llm-diff* buffer
    (delete-region start end)
    (goto-char start)
    (insert (shell-command-to-string diff-command))
    (smerge-mode)
    (diff-auto-refine-mode 1)

    (unless llm-preserve-temp-files
      (delete-file temp-file-before)
      (delete-file temp-file-after))))

(defun llm-complete-internal (prompt via model-type start end n-predict)
  "Completes text based on the current region using the LLM."
  (let* ((use-case "complete")
         (command (llm-infer-command-internal use-case via model-type "--n-predict" n-predict (llm-mode-text-type) prompt)))
    (message "llm-complete-internal: %s" command)
    (let ((old-text (buffer-substring start end)))
      (shell-command-on-region start end command nil t llm-error-buffer-name t nil)
      (goto-char start)
      (insert old-text))))

(defun llm-mode-text-type ()
  "Returns the name of the current major mode."
  (symbol-name major-mode))

(defun llm-load-model ()
  "Loads the specified model using the LLM script."
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

;;; Not yet implemented: Query Replace
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
