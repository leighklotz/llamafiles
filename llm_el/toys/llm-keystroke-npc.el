;;; llm-keystroke-npc.el

;; An Emacs function `m-x llm-keystroke-npc` to implement an NPC
;; (non-player character) that wakes up every 30 seconds and populates
;; a temporary buffer with the last 100 keystrokes, then calls `llm-ask`
;; on that buffer to ask it to make suggestions about Emacs usage based
;; on the keystrokes.

;; Define the variables to store keystrokes
(defvar llm-keystroke-buffer "*llm-keystroke-buffer*"
  "Buffer name for storing keystrokes.")

(defvar llm-keystroke-list nil
  "List to store captured keystrokes.")

(defvar llm-keystroke-timer nil
  "Timer for the NPC behavior.")

(defvar llm-keystroke-count 100
  "Number of keystrokes to capture.")

(defvar inhibit-llm-keystroke-npc nil
  "Set to non-nil to inhibit the 30-second timer function")

;; Function to capture keystrokes.
;; This function is called every time a keystroke is made.
(defun llm-capture-keystroke ()
  "Capture the last keystroke and store it in `llm-keystroke-list`."
  (interactive)
  ;; Capture the last keystroke
  (let* ((keystroke (this-command-keys))
         (keystroke-kbd (llm-kbd-sequence keystroke)))
    (setq llm-keystroke-list (nconc llm-keystroke-list (list keystroke-kbd)))
    ;; Ensure only the specified number of keystrokes are stored
    (when (> (length llm-keystroke-list) llm-keystroke-count)
      (setq llm-keystroke-list (nthcdr (- llm-keystroke-count 1) llm-keystroke-list)))))

;; Function to handle NPC behavior.
;; This function is called every 30 seconds by the timer.
(defun llm-keystroke-npc ()
  "Generate suggestions based on the captured keystrokes and display them."
  (interactive)
  (unless inhibit-llm-keystroke-npc
    (unless (get-buffer llm-keystroke-buffer)
      (with-current-buffer (get-buffer-create llm-keystroke-buffer)
        (erase-buffer)))
    (with-current-buffer llm-keystroke-buffer
      (erase-buffer)
      ;; Insert the keystrokes correctly formatted
      (let* ((keystrokes (format "Keystrokes:\n%s" (mapconcat #'identity llm-keystroke-list " ")))
             (prompt "Based on the keystrokes, suggest improvements for Emacs usage."))
        (insert keystrokes)
        (message prompt)
        (when (not (llm-ask prompt (point-min) (point-max)))
          (setq llm-keystroke-list nil))
        (message "NPC suggestions generated based on the last %d keystrokes: %s."
                 llm-keystroke-count
                 keystrokes)))))

;; Setup the timer to call `llm-keystroke-npc` every 30 seconds.
(defun llm-setup-keystroke-npc ()
  "Setup the timer to call `llm-keystroke-npc` every 30 seconds."
  (when llm-keystroke-timer
    (cancel-timer llm-keystroke-timer))
  (setq llm-keystroke-timer (run-at-time 30 nil 'llm-keystroke-npc)))

;; Ensure keystrokes are captured only in relevant buffers.
(defvar llm-capture-buffer-list nil
  "List of buffers in which keystrokes should be captured.")

(defun llm-npc-watch-buffer ()
  "Add a buffer to the list of buffers in which keystrokes should be captured."
  (interactive)
  (unless (memq (current-buffer) llm-capture-buffer-list)
    (push (current-buffer) llm-capture-buffer-list)))

(defun llm-kbd-sequence (key-seq)
  (apply #'concat
         (mapcar #'(lambda (x)
                     (with-temp-buffer
                       (describe-key-briefly x)
                       (buffer-string)))
                 key-seq)))

;; Setup the keystroke capture and timer.
(add-hook 'post-command-hook 'llm-capture-keystroke)
(llm-setup-keystroke-npc)
