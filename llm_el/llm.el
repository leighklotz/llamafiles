;;; -*-Mode: emacs-lisp -*-
;;; llm.el - LLM-based rewriting and summarization functions for Emacs buffers
;;;
;;; Copyright (C) 2024, 2025, 2026 Leigh Klotz <klotz@klotz.me>
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
;;; along with this program. If not, see <https://www.gnu.org/licenses/>.
;;;

;;; Core Functionality
;;; The package allows users to interact with an LLM using text from the 
;;; current buffer or a selected region. It supports several modes of interaction:
;;;
;;; * Generation & Transformation: Rewriting regions, summarizing buffers, 
;;;   writing new content based on prompts, and inserting responses directly at point.
;;; * Inquiry: Asking questions about specific regions or documentation 
;;;   (functions/variables) and receiving answers in new buffers.
;;; * Diff-based Editing: Some commands (`llm-rewrite`, `llm-todo`) replace 
;;;   text using a merge-file format, allowing users to review changes using 
;;;   `smerge-mode`.

;;; Key User Commands
;;; --- General LLM Tasks ---
;;; * `llm-summarize-buffer`: Summarizes the entire buffer into a new buffer.
;;; * `llm-rewrite`: Replaces a region with LLM-generated text (shows diff).
;;; * `llm-ask`: Asks a question about a region; results appear in a new buffer.
;;; * `llm-write`: Generates a response to a prompt based on a region into a new buffer.
;;; * `llm-insert`: Inserts the LLM's response directly at point.
;;; * `llm-quick`: A shortcut to ask a context-free question and insert a concise answer at point immediately.
;;; * `llm-complete`: (Experimental) Generates text to follow the current region.
;;; * `llm-todo`: Processes "todo" items in a region and replaces them with output.
;;; * `llm-unlx`: Utility to extract files from a "# file <name>" formatted buffer.
;;;
;;; Commands about Emacs
;;; These commands use `apropos`, `describe-function`, or `describe-variable` 
;;; to feed Emacs documentation into the LLM for explanation:
;;; * `llm-apropos`: Searches for symbols and asks the LLM about the results.
;;; * `llm-describe-function`: Asks questions about a specific function's documentation.
;;; * `llm-describe-variable`: Asks questions about a specific variable's documentation.
;;; * `llm-view-lossage`: Analyzes recent keystrokes to suggest Emacs usage improvements.
;;; * `llm-explain-output`: (Specifically for `comint-mode`) Explains the most 
;;;   recent shell command output/error.
;;; * `llm-vibe-emacs`: A specialized "vibe" rewrite mode.
;;;
;;; Utility & Configuration
;;; * `llm-load-model`: Interactively selects and loads a model via an external script.
;;;
;;; Configuration & Dependencies
;;; * Dependencies: Requires a shell script named `llm-emacs-helper.sh`.
;;; * Custom Variables: 
;;;   - `llm-rewrite-script-path`: Path to the primary rewrite script.
;;;   - `llm-via-script-path`: Path to the model management script.
;;;   - `llm-preserve-temp-files`: Controls whether temporary diff files are kept.
;;; * Buffers: Uses dedicated buffers for various outputs: 
;;;   `*llm-ask*`, `*llm-write*`, `*llm-summary*`, `*llm-errors*`, and `*llm-diff*`.
;;;
;;; Hooks & Keybindings
;;; * Hooks: Automatically attaches `llm-smerge-mode-hook` to `smerge-mode-hook`, 
;;;   providing convenient navigation keys (`M-up`/`M-down`) for reviewing LLM diffs.
;;; * Global Bindings: Provides several `M-s` prefixed global keybindings for key user commands

;;; Configuration

(defgroup llm nil
  "Configuration options for the LLM integration."
  :group 'emacs)

;;; Custom variables

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

;;; Buffer name variables

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

;;; Other configuration

(defvar llm-preserve-temp-files nil
  "Whether to preserve temporary files created during diffing.")

(defvar llm-git-merge-format "git merge-file -p \"%s\" /dev/null \"%s\""
  "Format string for the git merge command used in diffing.")

(defvar llm-prompt-history nil
  "History of prompts used with LLM functions.")

;;; Utility functions
(defun llm-get-user-prompt (label &optional default)
  "Prompt the user for a prompt, using `llm-prompt-history` for completion."
  (let ((completion-ignore-case t))
    (read-string label
                 (or default (if (not llm-prompt-history) "" (car llm-prompt-history) )))))

(defun llm-interactive-prompt (label)
  "Return a list containing a prompt from LABEL, and the current region bounds."
  (list (llm-get-user-prompt label)
        (if (use-region-p) (region-beginning) (point-min))
        (if (use-region-p) (region-end) (point-max))))

;;; User commands

(defun llm-ask (prompt &optional start end)
  "Write a new buffer based on PROMPT and the current region, or an empty string if no region is active.
The result is displayed in a buffer named \\[llm-ask-buffer-name]]."
  (interactive (llm-interactive-prompt "Ask: "))
  (push prompt llm-prompt-history)
  (llm-region-internal
   "ask" (llm-mode-text-type)
   prompt start end llm-ask-buffer-name nil nil))

(defun llm-summarize-buffer (prompt)
  "Summarize the entire current buffer using PROMPT and display the result in `llm-summary-buffer-name`."
  (interactive (list (llm-get-user-prompt "Summarize Buffer Prompt: ")))
  (push prompt llm-prompt-history)
  (llm-region-internal
   "summarize" (llm-mode-text-type)
   prompt (point-min) (point-max) llm-summary-buffer-name nil nil))

(defun llm-insert (prompt &optional start end)
  "Insert the LLM's response to PROMPT at point, using the selected region as input.
If no region is active, the input is the entire buffer."
  (interactive (llm-interactive-prompt "Insert Prompt: "))
  (unless (and start end)
    (setq start (if (use-region-p) (region-beginning) (point-min))
          end   (if (use-region-p) (region-end) (point-max))))
  (let ((llm-write-buffer-name t))      ; insert into current buffer
    (llm-region-internal
     "write" (llm-mode-text-type)
     prompt start end llm-write-buffer-name t nil)))

;;; Not yet implemented: llm-complete
(defun llm-complete (prompt start end)
  "Generate additional text following the selected region using PROMPT and insert it at END."
  (interactive (llm-interactive-prompt "Complete Prompt: "))
  (let ((n-predict 32))                 ; number of tokens to generate
    (push prompt llm-prompt-history)
    (llm-complete-internal prompt start end n-predict)))

(defun llm-write (prompt &optional start end)
  "Write a new buffer based on PROMPT and the current region, and display the result in \\[llm-write-buffer-name\\[."
  (interactive (llm-interactive-prompt "Write Prompt: "))
  (unless (and start end)
    (setq start (point-min)
          end   (point-min)))
  (push prompt llm-prompt-history)
  (llm-region-internal
   "write" (llm-mode-text-type)
   prompt start end llm-write-buffer-name nil nil))

(defun llm-rewrite (prompt start end)
  "Rewrite the selected region using PROMPT and the external LLM script.
The region is replaced with the LLM's output, and the changes are shown in merge‑file format."
  (interactive (llm-interactive-prompt "Rewrite Prompt: "))
  (push prompt llm-prompt-history)
  (llm-region-internal
   "rewrite" (llm-mode-text-type)
   prompt start end nil t t))

(defun llm-todo (prompt start end)
  "Process 'todo' items in the selected region using PROMPT and the external LLM script.
The region is replaced with the LLM's output, and the changes are shown in merge‑file format."
  (interactive (llm-interactive-prompt "Todo Prompt: "))
  (push prompt llm-prompt-history)
  (llm-region-internal
   "todo" (llm-mode-text-type)
   prompt start end nil t t))

;;; This function is used in comint-mode to understand and explain the output in an
;;; interactive way. It prompts the user with a default question, "What line
;;; number contains the proximal error?", or a custom prompt if provided. It
;;; then uses the output between the last two output boundaries to generate an
;;; explanation through the llm-ask function.
(defun llm-explain-output (prompt)
  "Explain the most recent comint output using PROMPT.
If PROMPT is omitted or empty, the default question \"What line number contains the proximal error?\" is used."
  (interactive (list (llm-get-user-prompt "Explain Output Prompt: ")))
  (let* ((bounds (my-comint-get-previous-output-bounds))
         (start  (car bounds))
         (end    (cadr bounds))
         (default-prompt "What line number contains the proximal error?")
         (prompt (if (or (null prompt) (string= "" prompt))
                     default-prompt
                   prompt)))
    (push prompt llm-prompt-history)
    (llm-ask prompt start end)))

;;; Internal helper functions

(defun llm-infer-command-internal (&rest args)
  "Construct the command string for invoking the external LLM script.
ARGS are the arguments to be passed to the script; each argument is quoted
to prevent shell injection."
  (concat
   llm-rewrite-script-path
   " "
   (mapconcat #'(lambda (arg) (shell-quote-argument (format "%s" arg))) args " ")))

(defun llm-region-internal (use-case major-mode-name prompt start end output-buffer-name replace-p diff-p)
  "Run the external LLM script on the current buffer or the selected region.
The command is built from USE‑CASE, MAJOR‑MODE‑NAME, and PROMPT.
If START and END are nil, the entire buffer is used.
OUTPUT‑BUFFER‑NAME is the buffer where the script output will be sent; if nil, a new buffer is created.
If REPLACE‑P is non‑nil, the region is replaced with the script output (or with the string \"t\" if the original code is kept for compatibility).
If DIFF‑P is non‑nil and REPLACE‑P is non‑nil, the changes are shown in merge‑file format using \\[[smerge-mode]].
The function sets `max-mini-window-height` to 0.0 to suppress window height adjustments for large outputs."
  (let ((max-mini-window-height 0.0)
        (output-buffer-name (if replace-p (buffer-name (current-buffer)) output-buffer-name))
        (region-noncontiguous-p nil))
    (cond ((and replace-p diff-p)
           (llm-region-as-diff-internal start end (llm-infer-command-internal use-case major-mode-name prompt)))
          (t
           (shell-command-on-region start end
                                    (llm-infer-command-internal use-case major-mode-name prompt)
                                    output-buffer-name replace-p
                                    llm-error-buffer-name t region-noncontiguous-p)
           ;; If we just created/targeted an llm buffer, make it markdown
           (when (and output-buffer-name 
                      (string-match-p "^\\*llm-" (buffer-name (get-buffer output-buffer-name))))
             (with-current-buffer (get-buffer output-buffer-name)
               (markdown-mode)))))))

(defun llm-region-as-diff-internal (start end command)
  "Run the external LLM script on the region between START and END and pass the result to \\[[llm-diff-region-with-string]]."
  (let* ((original-string (buffer-substring start end))
         (llm-output
          (with-temp-buffer
            (insert original-string)
            (shell-command-on-region (point-min) (point-max) command (current-buffer) t llm-error-buffer-name t nil)
            (markdown-mode)
            (buffer-string))))
    (message "llm-diff-region-with-string %d %d %s" start end llm-output)
    (llm-diff-region-with-string start end llm-output)))

(defun llm-diff-region-with-string (start end new-string)
  "Show the differences between the current buffer region and NEW-STRING in merge‑file format.
The differences are inserted into a temporary buffer named `llm-diff-buffer-name` and displayed in \\[[smerge-mode]]."
  (let* ((current-buffer-content (buffer-substring-no-properties start end))
         (temp-file-before (make-temp-file "emacs-diff-before-"))
         (temp-file-after  (make-temp-file "emacs-diff-after-"))
         (diff-command     (format llm-git-merge-format temp-file-before temp-file-after)))
    (message "diff-command %s" diff-command)

    (with-temp-file temp-file-before
      (insert current-buffer-content))

    (with-temp-file temp-file-after
      (insert new-string))

    ;; Run the diff command and capture the output in the *llm-diff* buffer
    (let ((result (shell-command-to-string diff-command)))
      (delete-region start end)
      (insert result)
      (smerge-mode)
      (diff-auto-refine-mode 1)
      (goto-char (point-min)))

    (unless llm-preserve-temp-files
      (delete-file temp-file-before)
      (delete-file temp-file-after))))

;;; Not yet implemented: llm-complete
(defun llm-complete-internal (prompt start end n-predict)
  "Generate additional text following the region from START to END using PROMPT.
The external script is called with the \"complete\" use‑case and the specified MODEL‑TYPE.
The number of tokens to generate is N‑PREDICT."
  (let* ((use-case "complete")
         (command (llm-infer-command-internal "complete" "--n-predict" n-predict (llm-mode-text-type) prompt)))
    (message "llm-complete-internal: %s" command)
    (let ((old-text (buffer-substring start end)))
      (shell-command-on-region start end command nil t llm-error-buffer-name t nil)
      (goto-char start)
      (insert old-text))))

(defun llm-mode-text-type ()
  "Return the name of the current major mode as a string."
  (symbol-name major-mode))

(defun llm-load-model ()
  "Interactively load a model using the external LLM script.
The user is prompted to select a model name from the list of available models
returned by the script."
  (interactive)
  (let* ((model-names (with-temp-buffer
                        (when (call-process llm-via-script-path nil t nil "--list-models")
                          (split-string (buffer-string) "\n" t))))
         (model-name (completing-read "Model: " model-names)))
    (when model-name
      (let ((results
             (with-temp-buffer
               (call-process llm-via-script-path nil t nil "--load-model" model-name)
               (split-string (buffer-string) "\n" t))))
        (message "model-name=%s results=%s" model-name results)))))

(defun my-comint-get-previous-output ()
  "Return the output of the previous shell command in comint mode."
  (interactive)
  (let* ((bounds (my-comint-get-previous-output-bounds))
         (start  (car bounds))
         (end    (cadr bounds)))
    (buffer-substring-no-properties start end)))

(defun my-comint-get-previous-output-bounds ()
  "Return the bounds of the output from the most recent shell command in comint mode."
  (let ((start (save-excursion
                 (comint-previous-prompt 1)
                 (forward-line 1)
                 (point)))
        (end (point-max)))
    (list start end)))

;;; Not yet implemented: Query Replace
(defun llm-query-replace (regex prompt)
  "Query replace occurrences matching REGEX with text generated by LLM using PROMPT.

This function is similar to \\[[query-replace-regexp]], but instead of replacing
with a fixed string, it uses an LLM to generate the replacement text based on
the provided prompt and the matched text.

Parameters:
- REGEX: A regular expression string to match text in the buffer.
- PROMPT: A string that serves as a prompt for the LLM. It should contain
          a `%s` placeholder, which will be replaced by the matched text.

Usage:
1. Call the function with M-x llm-query-replace.
2. Enter the regular expression when prompted.
3. Enter the LLM prompt, ensuring it includes `%s` where the text will be inserted.
4. The function will find all occurrences of the regex in the buffer.
5. For each match, it will ask if you want to replace it with LLM-generated text.
6. If you agree, it will generate the replacement text using the LLM and replace the match.

Note:
- Ensure that the LLM function \\[[llm-function]] is already defined and available.
- The replacement is undoable using the standard undo functionality in Emacs."
  (interactive "sRegex: \nsLLM Prompt: ")
  (let ((replacer (lambda (match)
                    (let* ((llm-input (format prompt match))
                           (llm-output (llm-function llm-input))) ; Assume llm-function is already defined
                      llm-output))))
    (query-replace-regexp regex replacer)))

;;; TODO: llm-quick should not insert a trailing newline. If that's too hard, have llm-quick remove a trailing newline after llm-insert.
(defun llm-quick (question)
  "Ask a short question and insert a concise answer at point.
The function prompts for QUESTION, then calls \\[[llm-insert]] with a prompt
that instructs the LLM to write a brief response to QUESTION and nothing else."
  (interactive "sQuestion: ")
  ;; provide no input
  (let ((current-point (point)))
    (llm-insert (format "Briefly write %s and nothing else." question) 
                current-point current-point)
    (when (bolp) (delete-char -1))))

(defun llm-apropos (apropos-match question)
  "Search for symbols matching APPROPOS-MATCH and ask the LLM a QUESTION about the results.
The function first calls \\[apropos]] to display matching symbols, then runs \\[[llm-ask]] with QUESTION on the displayed information."
  (interactive "sApropos: \nsQuestion: ")
  (save-excursion
    (apropos apropos-match)
    (llm-ask question (point-min) (point-max))))

(defun llm-describe-function (function-name question)
  "Display the documentation for FUNCTION-NAME and ask the LLM a QUESTION about it.
The function calls \\[[describe-function]] and then runs \\[[llm-ask]] with QUESTION on the resulting help buffer."
  (interactive
   (list (intern (completing-read "Function: " obarray 'fboundp))
         (read-string "Question: ")))
  (save-excursion
    (describe-function function-name)
    (set-buffer (help-buffer))
    (llm-ask question (point-min) (point-max))))

(defun llm-describe-variable (variable-name question)
  "Display the documentation for VARIABLE-NAME and ask the LLM a QUESTION about it.
The function calls \\[[describe-variable]] and then runs \\[[llm-ask]] with QUESTION on the resulting help buffer."
  (interactive
   (list (intern (completing-read "Variable: " obarray 'boundp))
         (read-string "Question: ")))
  (save-excursion
    (describe-variable variable-name)
    (set-buffer (help-buffer))
    (llm-ask question (point-min) (point-max))))

(defun llm-view-lossage (prompt)
  "Generate suggestions based on the captured keystrokes and display them."
  (interactive (list (llm-get-user-prompt
                      "Advice: "
                      "Based on the keystrokes, suggest improvements for Emacs usage.")))
  (view-lossage)
  (with-current-buffer (help-buffer) 
    (llm-ask prompt (point-min) (point-max))))

;;; Emacs Vibe
(defun llm-vibe-emacs (prompt start end)
  "Rewrite the selected region using PROMPT and the external LLM script.
The region is replaced with the LLM's output, and the changes are shown in merge‑file format."
  (interactive (llm-interactive-prompt "Vibe Prompt: "))
  (push prompt llm-prompt-history)
  (llm-region-internal "vibe-emacs" (llm-mode-text-type) prompt start end "*vibe-emacs*" nil nil))


;;; utils
(defun llm-unlx ()
  "Extracts a file from a 'lx' archive in the current buffer (or region) to a string.
The archive format is assumed to be lines starting with '# file <filename>'
followed by a fenced code block (lines starting with '```') containing the file content."
  (interactive)
  (let* ((target-file (read-string "Filename to extract: "))
         (buffer-string (buffer-string))
         (in-fence nil)
         (want nil)
         (buf "")
         (result ""))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (cond
           ((string-prefix-p "# file" line)
            (let ((marker (replace-regexp-in-string "# file[[:space:]]+" "" line)))
              (setq marker (string-trim marker))
              (setq want (string= marker target-file)))
            (forward-line 1))
           ((string= line "```")
            (if (not in-fence)
                (progn
                  (setq in-fence t)
                  (setq buf ""))
              (progn
                (setq in-fence nil)
                (if want
                    (setq result (concat result buf)))
                (setq want nil)))
            (forward-line 1))
           (in-fence
            (setq buf (concat buf line "\n"))
            (forward-line 1)))
          )
        )
      )

    (if result
        (message "%s" result)
      (message "File '%s' not found in the buffer." target-file))))
;;; Keybindings

(global-set-key (kbd "M-s a") 'llm-ask)
(global-set-key (kbd "M-s $") 'llm-summarize-buffer)
(global-set-key (kbd "M-s r") 'llm-rewrite)
(global-set-key (kbd "M-s x") 'llm-explain-output)
(global-set-key (kbd "M-s t") 'llm-todo)
(global-set-key (kbd "M-s i") 'llm-insert)
(global-set-key (kbd "M-s c") 'llm-complete)
(global-set-key (kbd "M-s v") 'llm-vibe-emacs)
(global-set-key (kbd "M-s q") 'llm-quick)

;;; Hooks
(defun llm-smerge-mode-hook ()
  "Set up convenient keybindings for \\[[smerge-mode]]."
  (local-set-key [M-down] 'smerge-next)
  (local-set-key [M-up] 'smerge-prev)
  (local-set-key [C-down] 'smerge-keep-lower)
  (local-set-key [C-up] 'smerge-keep-upper))
  
(add-hook 'smerge-mode-hook 'llm-smerge-mode-hook)

(provide 'llm)

