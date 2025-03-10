;;; llm.el - LLM-based rewriting and summarization functions for Emacs buffers

;; todo:  write an emacs function m-x `llm-keystroke-npc` to implement an NPC (non-player character) that wakes up every 30 seconds
;; and populates a temp buffer with the last 100 keystrokes, then calls llm-ask on that buffer to ask it
;; to make suggestions about emacs usage based on the keystrokes

;; Define the variables to store keystrokes
(defvar llm-keystroke-buffer "*llm-keystroke-buffer*")
(defvar llm-keystroke-list ())
(defvar llm-keystroke-timer nil)

;; Function to capture keystrokes
(defun llm-capture-keystroke ()
  (interactive)
  ;; Capture the last keystroke
  (let ((keystroke (this-command-keys-vector)))
    (setq llm-keystroke-list (nconc llm-keystroke-list (list keystroke)))
    ;; Ensure only the last 100 keystrokes are stored
    (if (>= (length llm-keystroke-list) 100)
        (setq llm-keystroke-list (cdr llm-keystroke-list)))))

;; Function to handle NPC behavior
(defun llm-keystroke-npc ()
  (interactive)
  (with-temp-buffer
    (erase-buffer)
    (insert (format "Keystrokes:\n%s" (mapconcat #'identity llm-keystroke-list " ")))
    (let ((prompt "Based on the keystrokes, suggest improvements for Emacs usage."))
      (llm-ask prompt (point-min) (point-max))))
  (setq llm-keystroke-list ())
  (message "NPC suggestions generated based on the last 100 keystrokes."))

;; Setup the timer to call llm-keystroke-npc every 30 seconds
(defun llm-setup-keystroke-npc ()
  (if llm-keystroke-timer
      (cancel-timer llm-keystroke-timer))
  (setq llm-keystroke-timer (run-at-time 30 nil 'llm-keystroke-npc)))

;; Bind the keystrokes to capture function
(defun llm-bind-keys-to-capture ()
  (global-set-key (kbd "<any>") 'llm-capture-keystroke))

;; Setup the keystroke capture and timer
(llm-bind-keys-to-capture)
(llm-setup-keystroke-npc)

;; Ensure keystrokes are captured only in relevant buffers
(defvar llm-capture-buffer-list '())
(defun llm-set-buffer-list (buf)
  (when (not (memq buf llm-capture-buffer-list))
    (push buf llm-capture-buffer-list)))
(add-hook 'post-command-hook 'llm-capture-keystroke) 
