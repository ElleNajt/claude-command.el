;;; claude-command.el --- Org mode notification queue for Claude Command -*- lexical-binding: t; -*-

;; Author: Claude AI
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.0") (org "9.0"))
;; Keywords: tools, ai, org

;;; Commentary:
;; This package extends claude-command.el with org mode notification queue functionality.
;; It provides persistent task tracking in ~/.claude/taskmaster.org with timestamps
;; and clickable buffer links, plus smart popup notifications that only appear when
;; the Claude buffer isn't currently visible.

;;; Code:

(require 'json)
(require 'cl-lib)

;; Forward declarations for claude-command functions
;; Note: claude-code-handle-hook is provided by the claude-code package
(declare-function claude-code-handle-hook "claude-code")
(defvar claude-code-event-hook)

;; Declare functions from perspective.el
(declare-function persp-names "persp-mode")
(declare-function persp-get-by-name "persp-mode")
(declare-function persp-switch "persp-mode")
(declare-function persp-buffers "persp-mode")

;; Declare functions from org-mode
(declare-function org-back-to-heading "org")
(declare-function org-next-visible-heading "org")

;; Declare functions from evil (optional)
(declare-function evil-insert-state "evil-states")

;; Constants
(defconst claude-command-notification-buffer-name "*Claude Command Notification*"
  "Name of the notification buffer.")

(defconst claude-command-org-todo-pattern "^\* TODO Claude task completed"
  "Pattern to match Claude task entries in org file.")

;;;; Customization

(defcustom claude-command-taskmaster-org-file (expand-file-name "~/.claude/taskmaster.org")
  "Path to the org mode file for storing Claude task notifications.

This file will contain a queue of completed Claude tasks as TODO entries
with timestamps and links back to the original Claude buffers."
  :type 'file
  :group 'claude-command)

(defcustom claude-command-auto-advance-queue nil
  "Whether to automatically advance to the next queue entry after sending input.

When non-nil, pressing enter (or sending any input) in a Claude buffer will:
1. Clear the current buffer from the task queue
2. Automatically switch to the next Claude buffer in the queue

This provides a streamlined workflow for processing multiple completed tasks."
  :type 'boolean
  :group 'claude-command)

(defcustom claude-command-notification-style 'message
  "How to display Claude task completion notifications.

'popup - Show notifications in a popup window at the bottom
'message - Show notifications in the echo area (minibuffer)
'both - Show both popup and message notifications
'none - Disable notifications entirely"
  :type '(choice (const :tag "Popup window" popup)
                 (const :tag "Echo area message" message)
                 (const :tag "Both popup and message" both)
                 (const :tag "No notifications" none))
  :group 'claude-command)

;;;; Modeline notification support

(defvar claude-command--modeline-notification nil
  "Current modeline notification string, or nil if none.")

(defvar claude-command--modeline-notification-timer nil
  "Timer for auto-clearing modeline notifications.")

(defun claude-command--show-modeline-notification (message &optional timeout)
  "Show MESSAGE in the modeline for TIMEOUT seconds (default 10)."
  (setq claude-command--modeline-notification 
        (propertize (format " [Claude: %s]" message)
                    'face 'mode-line-emphasis))
  (force-mode-line-update t)
  
  ;; Cancel any existing timer
  (when claude-command--modeline-notification-timer
    (cancel-timer claude-command--modeline-notification-timer))
  
  ;; Set new timer to clear notification
  (setq claude-command--modeline-notification-timer
        (run-with-timer (or timeout 10) nil
                        'claude-command--clear-modeline-notification)))

(defun claude-command--clear-modeline-notification ()
  "Clear the current modeline notification."
  (setq claude-command--modeline-notification nil)
  (setq claude-command--modeline-notification-timer nil)
  (force-mode-line-update t))

;;;; Modeline integration

(defun claude-command--setup-modeline ()
  "Add Claude notifications to the modeline."
  ;; For regular modeline (non-doom-modeline)
  (unless (memq 'claude-command--modeline-notification global-mode-string)
    (setq global-mode-string 
          (append global-mode-string '(claude-command--modeline-notification)))))

;;;; Org mode integration functions

(defun claude-command--ensure-claude-directory ()
  "Ensure the Claude directory exists for storing taskmaster.org."
  (let ((claude-dir (file-name-directory claude-command-taskmaster-org-file)))
    (unless (file-directory-p claude-dir)
      (make-directory claude-dir t))))

(defun claude-command--format-org-timestamp ()
  "Format current time as an org mode timestamp."
  (format-time-string "[%Y-%m-%d %a %H:%M]"))

(defun claude-command--get-workspace-from-buffer-name (buffer-name)
  "Extract workspace directory from Claude BUFFER-NAME.
For example, *claude:/path/to/project/* returns /path/to/project/."
  (when (and buffer-name (string-match "^\\*claude:\\([^:]+\\)\\(?::\\([^*]+\\)\\)?\\*$" buffer-name))
    (match-string 1 buffer-name)))

(defun claude-command--add-org-todo-entry (buffer-name message)
  "Add a TODO entry to the taskmaster org file.

BUFFER-NAME is the name of the Claude buffer that completed a task.
MESSAGE is the notification message to include in the TODO entry.

If an entry for the same buffer already exists, it will be removed first
to prevent duplicate entries in the queue."
  (claude-command--ensure-claude-directory)
  ;; First, remove any existing entry for this buffer
  (when buffer-name
    (claude-command--delete-queue-entry-for-buffer buffer-name))
  
  (let* ((timestamp (claude-command--format-org-timestamp))
         (buffer-link (if buffer-name
                          (format "[[elisp:(switch-to-buffer \"%s\")][%s]]" buffer-name buffer-name)
                        "Unknown buffer")))
    (with-temp-buffer
      (when (file-exists-p claude-command-taskmaster-org-file)
        (insert-file-contents claude-command-taskmaster-org-file))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "* TODO Claude task completed %s\n" timestamp))
      (insert (format "  Message: %s\n" (or message "Task completed")))
      (insert (format "  Buffer: %s\n" buffer-link))
      (insert (format "  Actions: [[elisp:(claude-command--switch-to-workspace-for-buffer \"%s\")][Go to Workspace]] | [[elisp:(claude-command--clear-current-org-entry-and-switch \"%s\")][Clear and Go to Workspace]]\n" buffer-name buffer-name))
      (insert "\n")
      ;; Suppress "Wrote file" messages with silent write
      (write-region (point-min) (point-max) claude-command-taskmaster-org-file nil 'silent))))

(defun claude-command--get-most-recent-buffer ()
  "Get the most recent Claude buffer name from the taskmaster org file."
  (when (file-exists-p claude-command-taskmaster-org-file)
    (with-temp-buffer
      (insert-file-contents claude-command-taskmaster-org-file)
      (goto-char (point-max))
      (when (re-search-backward "Buffer: \\[\\[elisp:(switch-to-buffer \"\\([^\"]+\\)\")\\]\\[" nil t)
        (match-string 1)))))

(defun claude-command--find-workspace-for-buffer (buffer-name)
  "Find the perspective that contains the specified BUFFER-NAME."
  (when (featurep 'persp-mode)
    (let ((target-buffer (get-buffer buffer-name)))
      (when target-buffer
        (cl-loop for persp-name in (persp-names)
                 for persp = (persp-get-by-name persp-name)
                 when (and persp
                           (member target-buffer (persp-buffers persp)))
                 return persp-name)))))

(defun claude-command--switch-to-workspace-for-buffer (buffer-name)
  "Switch to the perspective that contains BUFFER-NAME and navigate to it."
  (let ((target-buffer (get-buffer buffer-name)))
    (cond
     ;; Buffer doesn't exist - warn but don't error
     ((not target-buffer)
      (message "Warning: Buffer '%s' no longer exists - removing from queue" buffer-name)
      (claude-command--delete-queue-entry-for-buffer buffer-name)
      nil)
     ;; Buffer exists, try to find its perspective
     (t
      (if-let ((persp-name (claude-command--find-workspace-for-buffer buffer-name)))
          (progn
            ;; Found perspective, switch to it
            (message "Switching to perspective: %s for buffer: %s" persp-name buffer-name)
            (persp-switch persp-name)
            ;; Give perspective a moment to switch
            (run-with-timer 0.1 nil
                            (lambda ()
                              (let ((buf (get-buffer buffer-name)))
                                (when buf
                                  (if-let ((window (get-buffer-window buf)))
                                      ;; Buffer is visible, just select the window
                                      (select-window window)
                                    ;; Buffer is not visible, display it
                                    (switch-to-buffer buf))
                                  ;; If using evil mode and this is a Claude buffer, enter insert mode
                                  (when (and (boundp 'evil-mode) evil-mode
                                             (string-match-p "^\\*claude:" buffer-name))
                                    (evil-insert-state))))))
            (message "Switched to perspective: %s and navigated to buffer: %s" persp-name buffer-name)
            persp-name)
        ;; Buffer exists but no perspective found - just switch to buffer
        (progn
          (message "No perspective found for buffer '%s' - switching to buffer directly" buffer-name)
          (switch-to-buffer target-buffer)
          ;; If using evil mode and this is a Claude buffer, enter insert mode
          (when (and (boundp 'evil-mode) evil-mode
                     (string-match-p "^\\*claude:" buffer-name))
            (evil-insert-state))
          nil))))))

(defun claude-command--clear-most-recent-org-entry ()
  "Clear (mark as DONE) the most recent TODO entry in the taskmaster org file."
  (when (file-exists-p claude-command-taskmaster-org-file)
    (with-temp-buffer
      (insert-file-contents claude-command-taskmaster-org-file)
      (goto-char (point-max))
      (when (re-search-backward claude-command-org-todo-pattern nil t)
        (replace-match "* DONE Claude task completed")
        ;; Suppress "Wrote file" messages with silent write
        (write-region (point-min) (point-max) claude-command-taskmaster-org-file nil 'silent)))))

(defun claude-command--clear-current-org-entry-and-switch (buffer-name)
  "Delete the current TODO entry and switch to workspace for BUFFER-NAME."
  (interactive)
  (when (and (buffer-file-name)
             (string= (file-name-nondirectory (buffer-file-name)) "taskmaster.org"))
    ;; We're in the taskmaster.org file, delete current entry
    (save-excursion
      (org-back-to-heading t)
      (when (looking-at claude-command-org-todo-pattern)
        ;; Delete the entire entry (from heading to next heading or end of buffer)
        (let ((start (point)))
          (if (org-next-visible-heading 1)
              (delete-region start (point))
            (delete-region start (point-max))))
        (save-buffer)
        (message "Deleted entry and switching to workspace...")
        ;; Switch to workspace
        (claude-command--switch-to-workspace-for-buffer buffer-name)))))

;;;; Notification dismissal system

(defvar claude-command--notification-dismiss-active nil
  "Whether notification dismiss mode is currently active.")

(defvar claude-command--notification-buffer-name nil
  "Name of the current notification buffer.")

(defun claude-command--enable-notification-dismiss (buffer-name)
  "Enable global notification dismissal for BUFFER-NAME."
  (unless claude-command--notification-dismiss-active
    (setq claude-command--notification-dismiss-active t
          claude-command--notification-buffer-name buffer-name)
    ;; Use overriding-local-map for higher precedence
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<escape>") 'claude-command--dismiss-notification-if-visible)
      (define-key map (kbd "q") 'claude-command--dismiss-notification-if-visible)
      (setq overriding-local-map map))))

(defun claude-command--disable-notification-dismiss ()
  "Disable global notification dismissal and restore original ESC binding."
  (when claude-command--notification-dismiss-active
    (setq claude-command--notification-dismiss-active nil
          claude-command--notification-buffer-name nil)
    ;; Clear the overriding map
    (setq overriding-local-map nil)))

(defun claude-command--dismiss-notification-if-visible ()
  "Dismiss notification if visible."
  (interactive)
  (when (and claude-command--notification-dismiss-active
             claude-command--notification-buffer-name
             (get-buffer-window claude-command--notification-buffer-name))
    ;; Notification is visible, dismiss it
    (kill-buffer claude-command--notification-buffer-name)
    (claude-command--disable-notification-dismiss)))

(defun claude-command--dismiss-and-kill-buffer (buffer-name)
  "Helper to dismiss notification and kill BUFFER-NAME."
  (claude-command--disable-notification-dismiss)
  (kill-buffer buffer-name))

;;;; PreToolUse Hook System

(defvar claude-command--active-pre-tool-use-timer nil
  "Timer for automatically clearing PreToolUse minibuffer prompts.")

(defvar claude-command--pre-tool-use-active nil
  "Whether a PreToolUse prompt is currently active in the minibuffer.")

(defun claude-command--handle-pre-tool-use (buffer-name json-data)
  "Handle PreToolUse hook events to display input requests in minibuffer.

BUFFER-NAME is the Claude buffer name.
JSON-DATA contains the tool use information that needs permission."
  (when json-data
    (condition-case err
        (let* ((data (json-read-from-string json-data))
               (tool-name (cdr (assq 'tool_name data)))
               (tool-args (cdr (assq 'tool_arguments data)))
               (prompt-text (format "Claude wants to use %s%s - Allow? (y/n/q): " 
                                   (or tool-name "unknown tool")
                                   (if tool-args 
                                       (format " with args: %s" 
                                              (if (stringp tool-args)
                                                  tool-args
                                                (json-encode tool-args)))
                                     ""))))
          ;; Clear any existing timer
          (when claude-command--active-pre-tool-use-timer
            (cancel-timer claude-command--active-pre-tool-use-timer)
            (setq claude-command--active-pre-tool-use-timer nil))
          
          ;; Set flag to indicate active prompt
          (setq claude-command--pre-tool-use-active t)
          
          ;; Show prompt in minibuffer with async input
          (run-with-timer 0.1 nil 
                         (lambda ()
                           (claude-command--show-pre-tool-use-prompt prompt-text buffer-name data))))
      (error
       (message "Error parsing PreToolUse JSON: %s" (error-message-string err))))))

(defun claude-command--show-pre-tool-use-prompt (prompt-text buffer-name data)
  "Show PreToolUse prompt in minibuffer with PROMPT-TEXT for BUFFER-NAME and DATA."
  (let ((response (read-char-choice prompt-text '(?y ?n ?q ?Y ?N ?Q))))
    (setq claude-command--pre-tool-use-active nil)
    (case response
      ((?y ?Y)
       (claude-command--send-pre-tool-use-response "allow" "User approved via minibuffer" buffer-name)
       (message "Tool use approved"))
      ((?q ?Q)
       (claude-command--send-pre-tool-use-response "ask" "User requested UI confirmation" buffer-name)
       (message "Requesting UI confirmation"))
      ((?n ?N)
       (claude-command--send-pre-tool-use-response "deny" "User denied via minibuffer" buffer-name)
       (message "Tool use denied"))
      (t
       (claude-command--send-pre-tool-use-response "ask" "Invalid response, requesting UI confirmation" buffer-name)
       (message "Invalid response, requesting UI confirmation")))))

(defun claude-command--send-pre-tool-use-response (decision reason buffer-name)
  "Send PreToolUse response with DECISION and REASON for BUFFER-NAME.

DECISION should be 'allow', 'deny', or 'ask'.
REASON is an explanatory string."
  (let ((response-json (json-encode `((hookSpecificOutput . ((hookEventName . "PreToolUse")
                                                            (permissionDecision . ,decision)
                                                            (permissionDecisionReason . ,reason)))))))
    ;; Write the response to stdout so Claude Code can process it
    ;; This mimics what a shell hook would do
    (message "PreToolUse Response: %s" response-json)
    (with-temp-file "/tmp/claude-pretooluse-response.json"
      (insert response-json))
    ;; Also print to stdout for debugging
    (princ response-json)))

;;;; Enhanced notification system

(defun claude-command--buffer-visible-in-current-perspective-p (buffer-name)
  "Check if BUFFER-NAME is currently visible in the active perspective.

Returns t if the buffer is visible in a window in the current perspective,
nil otherwise."
  (when-let ((target-buffer (get-buffer buffer-name)))
    (and 
     ;; Buffer exists and is live
     (buffer-live-p target-buffer)
     ;; Buffer has a visible window
     (get-buffer-window target-buffer)
     ;; If persp-mode is active, check if we're in the right perspective
     (or (not (featurep 'persp-mode))
         (let ((buffer-persp (claude-command--find-workspace-for-buffer buffer-name))
               (current-persp (when (fboundp 'get-current-persp)
                                (let ((cp (get-current-persp)))
                                  (when cp (persp-name cp))))))
           ;; Either buffer has no perspective (global) or we're in its perspective
           (or (null buffer-persp)
               (string= buffer-persp current-persp)))))))

;;;###autoload
(defun claude-command-org-notification-listener (message)
  "Handle Claude Command hook events for org-mode task tracking.

MESSAGE is a plist with :type, :buffer-name, :json-data, and :args keys.
This is designed to work with the new claude-code-event-hook system."
  (let ((hook-type (plist-get message :type))
        (buffer-name (plist-get message :buffer-name))
        (json-data (plist-get message :json-data)))
    (cond
     ((eq hook-type 'notification)
      (claude-command--handle-task-completion buffer-name "Claude task completed" json-data))
     ((eq hook-type 'stop)
      (claude-command--handle-task-completion buffer-name "Claude session stopped" json-data))
     ((eq hook-type 'pre-tool-use)
      (claude-command--handle-pre-tool-use buffer-name json-data)))))

(defun claude-command--handle-task-completion (buffer-name message json-data)
  "Handle a Claude task completion event.

BUFFER-NAME is the name of the Claude buffer.
MESSAGE is the notification message to display and log.
JSON-DATA is the JSON payload from Claude CLI."
  (let* ((notification-buffer claude-command-notification-buffer-name)
         (target-buffer (when buffer-name (get-buffer buffer-name)))
         (has-workspace (and buffer-name 
                             (claude-command--get-workspace-from-buffer-name buffer-name)))
         (buffer-visible (claude-command--buffer-visible-in-current-perspective-p buffer-name)))

    ;; Always add entry to org file regardless of visibility
    (claude-command--add-org-todo-entry buffer-name message)
    
    ;; Show notifications based on style setting, but only if buffer is not currently visible
    (unless (or buffer-visible (eq claude-command-notification-style 'none))
      (let* ((queue-total (length (claude-command--get-all-queue-entries)))
             (notification-text (format "%s%s - Buffer: %s" 
                                        message
                                        (if (> queue-total 0)
                                            (format " (%d in queue)" queue-total)
                                          "")
                                        (or buffer-name "unknown buffer")))
             (short-notification (format "%s (%d in queue)" 
                                        (if (string-match "Claude \\(.*\\)" message)
                                            (match-string 1 message)
                                          message)
                                        queue-total)))
        
        ;; Show message notification
        (when (memq claude-command-notification-style '(message both))
          (message "Claude: %s" short-notification))
        
        ;; Show popup notification
        (when (memq claude-command-notification-style '(popup both))
          (with-current-buffer (get-buffer-create notification-buffer)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert notification-text)
              (goto-char (point-min))
              (setq buffer-read-only t))
            
            ;; Display as small popup without stealing focus
            (display-buffer notification-buffer 
                            '((display-buffer-in-side-window)
                              (side . bottom)
                              (window-height . 1)
                              (select . nil)))
            
            ;; Auto-dismiss after 2 seconds
            (run-with-timer 2 nil `(lambda ()
                                     (when (get-buffer ,notification-buffer)
                                       (kill-buffer ,notification-buffer))))))))
    
    ;; Disabled complex popup - keeping code for potential future use
    (when nil  ;; Change to t to re-enable complex popups
      (unless (and target-buffer 
                   (or (get-buffer-window target-buffer)
                       (eq (current-buffer) target-buffer)))
        ;; Create and display notification buffer
        (with-current-buffer (get-buffer-create notification-buffer)
          (let ((inhibit-read-only t)
                (queue-total (length (claude-command--get-all-queue-entries))))
            (erase-buffer)
            (insert (format "Claude notification: %s\n" (or message "Task completed")))
            (insert (format "Buffer: %s\n" (or buffer-name-override "unknown buffer")))
            ;; Add queue position information
            (when (> queue-total 0)
              (insert (format "Queue: %d entries\n" queue-total)))
            (insert "\n")

            (if (and target-buffer (buffer-live-p target-buffer))
                (insert-button "Switch to Claude buffer"
                               'action `(lambda (_button)
                                          (when (buffer-live-p ,target-buffer)
                                            (switch-to-buffer ,target-buffer)
                                            ;; Enter insert mode if using evil
                                            (when (and (boundp 'evil-mode) evil-mode
                                                       (string-match-p "^\\*claude:" ,buffer-name-override))
                                              (evil-insert-state))
                                            (claude-command--dismiss-and-kill-buffer ,notification-buffer)))
                               'help-echo (format "Click to switch to %s" buffer-name-override))
              (insert (format "Buffer '%s' not found or no longer exists." (or buffer-name-override "unknown"))))

            (insert "\n")
            (when has-workspace
              (insert-button "Open Workspace"
                             'action `(lambda (_button)
                                        (claude-command--switch-to-workspace-for-buffer ,buffer-name-override)
                                        (claude-command--dismiss-and-kill-buffer ,notification-buffer))
                             'help-echo (format "Click to switch to workspace for buffer: %s" buffer-name-override))
              (insert "   ")
              (insert-button "Open & Clear"
                             'action `(lambda (_button)
                                        (claude-command--switch-to-workspace-for-buffer ,buffer-name-override)
                                        (claude-command--clear-most-recent-org-entry)
                                        (claude-command--dismiss-and-kill-buffer ,notification-buffer))
                             'help-echo (format "Click to switch to workspace and clear org entry for buffer: %s" buffer-name-override))
              (insert "\n"))

            (insert "\n")
            (insert-button "View Task Queue"
                           'action `(lambda (_button)
                                      (find-file ,claude-command-taskmaster-org-file)
                                      (claude-command--dismiss-and-kill-buffer ,notification-buffer))
                           'help-echo "Click to view the org mode task queue")
            (insert "   ")
            (insert-button "Skip Entry"
                           'action `(lambda (_button)
                                      (claude-command--delete-queue-entry-for-buffer ,buffer-name-override)
                                      (claude-command--dismiss-and-kill-buffer ,notification-buffer)
                                      (message "Skipped queue entry for %s" ,buffer-name-override))
                           'help-echo "Click to skip this queue entry")

            (goto-char (point-min)))
          
          ;; Display the notification buffer and set up dismissal
          (display-buffer notification-buffer
                          '((display-buffer-in-side-window)
                            (side . bottom)
                            (window-height . 0.3)
                            (select . nil)))
          (claude-command--enable-notification-dismiss notification-buffer)
          
          ;; Auto-dismiss timer
          (run-with-timer 10 nil `(lambda ()
                                    (when (buffer-live-p (get-buffer ,notification-buffer))
                                      (claude-command--dismiss-and-kill-buffer ,notification-buffer)))))))))

;;;###autoload
(defun claude-command-test-notification ()
  "Test the notification system interactively."
  (interactive)
  (claude-command-org-notification-listener 
   (list :type 'notification 
         :buffer-name (buffer-name)
         :json-data "{\"test\": true}"
         :args '())))

;;;###autoload
(defun claude-command-test-pre-tool-use ()
  "Test the PreToolUse system interactively."
  (interactive)
  (claude-command-org-notification-listener 
   (list :type 'pre-tool-use 
         :buffer-name (buffer-name)
         :json-data "{\"tool_name\": \"Test Tool\", \"tool_arguments\": {\"arg1\": \"value1\"}}"
         :args '())))

;;;; Settings.json configuration helper

;;;###autoload
(defun claude-command-setup-hooks ()
  "Add or update Claude Command notification hooks in ~/.claude/settings.json."
  (interactive)
  (let* ((claude-dir (expand-file-name "~/.claude"))
         (settings-file (expand-file-name "settings.json" claude-dir))
         (emacsclient-cmd (executable-find "emacsclient"))
         (hooks-config `((hooks . ((Notification . [((matcher . "")
                                                     (hooks . [((type . "command")
                                                                (command . ,(format "%s --eval \"(claude-code-handle-hook 'notification \\\"$CLAUDE_BUFFER_NAME\\\")\" \"$(cat)\""
                                                                                    emacsclient-cmd)))]))])

                                   (Stop . [((matcher . "")
                                             (hooks . [((type . "command")
                                                        (command . ,(format "%s --eval \"(claude-code-handle-hook 'stop \\\"$CLAUDE_BUFFER_NAME\\\")\" \"$(cat)\""
                                                                            emacsclient-cmd)))]))])
                                   
                                   (PreToolUse . [((matcher . "")
                                                   (hooks . [((type . "command")
                                                              (command . ,(format "%s --eval \"(claude-code-handle-hook 'pre-tool-use \\\"$CLAUDE_BUFFER_NAME\\\")\" \"$(cat)\""
                                                                                  emacsclient-cmd)))]))])))))
         (existing-config (when (file-exists-p settings-file)
                            (condition-case err
                                (json-read-file settings-file)
                              (error
                               (message "Warning: Could not parse existing settings.json: %s" (error-message-string err))
                               nil))))
         (new-config (if existing-config
                         (let ((config-alist (if (hash-table-p existing-config)
                                                 (claude-command--hash-table-to-alist existing-config)
                                               existing-config)))
                           (claude-command--merge-hooks-config config-alist hooks-config))
                       hooks-config)))

    (unless emacsclient-cmd
      (error "emacsclient not found in PATH. Please ensure Emacs server is properly installed"))

    ;; Ensure Claude directory exists
    (unless (file-directory-p claude-dir)
      (make-directory claude-dir t))

    ;; Write updated config with pretty formatting
    (with-temp-file settings-file
      (let ((json-encoding-pretty-print t))
        (insert (json-encode new-config))))

    (message "Claude Command notification hooks added to %s" settings-file)))

(defun claude-command--hash-table-to-alist (hash-table)
  "Convert HASH-TABLE to an alist."
  (let (result)
    (maphash (lambda (key value)
               (push (cons key value) result))
             hash-table)
    (nreverse result)))

(defun claude-command--merge-hooks-config (existing-config hooks-config)
  "Merge HOOKS-CONFIG into EXISTING-CONFIG, preserving other settings."
  (let ((config-copy (copy-alist existing-config))
        (hooks-entry (assoc 'hooks hooks-config)))
    (if (assoc 'hooks config-copy)
        ;; Hooks section exists, merge it
        (setcdr (assoc 'hooks config-copy) (cdr hooks-entry))
      ;; No hooks section, add it
      (push hooks-entry config-copy))
    config-copy))

;;;; Workspace Navigation Commands

(defvar claude-command-previous-workspace nil
  "The previous workspace before navigating to a Claude buffer.")

(defun claude-command--record-previous-workspace ()
  "Record the current workspace before switching."
  (when (featurep 'persp-mode)
    (let ((current-persp (get-current-persp)))
      (when current-persp
        (setq claude-command-previous-workspace (persp-name current-persp))))))

;;;###autoload
(defun claude-command-goto-recent-workspace ()
  "Go to the most recent perspective from the taskmaster org file."
  (interactive)
  (claude-command--record-previous-workspace)
  (if-let ((buffer-name (claude-command--get-most-recent-buffer)))
      (claude-command--switch-to-workspace-for-buffer buffer-name)
    (message "No recent perspective found in taskmaster.org")))

;;;###autoload
(defun claude-command-goto-recent-workspace-and-clear ()
  "Go to the most recent perspective and clear the org entry."
  (interactive)
  (claude-command--record-previous-workspace)
  (if-let ((buffer-name (claude-command--get-most-recent-buffer)))
      (progn
        (claude-command--switch-to-workspace-for-buffer buffer-name)
        (claude-command--clear-most-recent-org-entry)
        (message "Switched to perspective and cleared org entry for buffer: %s" buffer-name))
    (message "No recent perspective found in taskmaster.org")))

;;;###autoload
(defun claude-command-return-to-previous ()
  "Return to the previous workspace before last queue navigation."
  (interactive)
  (if claude-command-previous-workspace
      (progn
        (persp-switch claude-command-previous-workspace)
        (message "Returned to workspace: %s" claude-command-previous-workspace))
    (message "No previous workspace recorded")))


;;;; Queue Navigation System

(defvar claude-command--queue-position 0
  "Current position in the taskmaster.org queue.")

(defun claude-command--get-all-queue-entries ()
  "Get all TODO entries from taskmaster.org as a list of buffer names."
  (when (file-exists-p claude-command-taskmaster-org-file)
    (with-temp-buffer
      (insert-file-contents claude-command-taskmaster-org-file)
      (goto-char (point-min))
      (let (entries)
        (while (re-search-forward claude-command-org-todo-pattern nil t)
          (when (re-search-forward "Buffer: \\[\\[elisp:(switch-to-buffer \"\\([^\"]+\\)\")\\]\\[" nil t)
            (push (match-string 1) entries)))
        (nreverse entries)))))

(defun claude-command--get-queue-entry-at-position (position)
  "Get the queue entry at POSITION, or nil if out of bounds."
  (let ((entries (claude-command--get-all-queue-entries)))
    (when (and entries (>= position 0) (< position (length entries)))
      (nth position entries))))

(defun claude-command--delete-queue-entry-for-buffer (buffer-name)
  "Delete ALL queue entries corresponding to BUFFER-NAME from taskmaster.org."
  (when (file-exists-p claude-command-taskmaster-org-file)
    (let ((lines '())
          (current-entry-lines '())
          (in-matching-entry nil)
          (deleted-count 0))
      (with-temp-buffer
        (insert-file-contents claude-command-taskmaster-org-file)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties 
                       (line-beginning-position) 
                       (line-end-position))))
            (cond
             ;; Start of a new TODO entry
             ((string-match claude-command-org-todo-pattern line)
              ;; Save previous entry if it wasn't matching
              (when (and current-entry-lines (not in-matching-entry))
                (setq lines (append lines current-entry-lines)))
              (when in-matching-entry
                (setq deleted-count (1+ deleted-count)))
              ;; Start new entry
              (setq current-entry-lines (list line))
              (setq in-matching-entry nil))
             ;; Check if this line contains our buffer name
             ((and current-entry-lines 
                   (string-match (regexp-quote buffer-name) line))
              (setq in-matching-entry t)
              (setq current-entry-lines (append current-entry-lines (list line))))
             ;; Regular line in current entry
             (current-entry-lines
              (setq current-entry-lines (append current-entry-lines (list line))))
             ;; Line outside any entry
             (t
              (setq lines (append lines (list line)))))
            (forward-line 1)))
        ;; Handle final entry
        (when current-entry-lines
          (if in-matching-entry
              (setq deleted-count (1+ deleted-count))
            (setq lines (append lines current-entry-lines)))))
      ;; Write back to file
      (with-temp-buffer
        (dolist (line lines)
          (insert line "\n"))
        ;; Suppress "Wrote file" messages with silent write
        (write-region (point-min) (point-max) claude-command-taskmaster-org-file nil 'silent))
      (> deleted-count 0))))

;;;###autoload
(defun claude-command-queue-next ()
  "Navigate to the next entry in the taskmaster.org queue."
  (interactive)
  (claude-command--record-previous-workspace)
  (let* ((entries (claude-command--get-all-queue-entries))
         (total (length entries)))
    (if (zerop total)
        (message "No entries in queue")
      (setq claude-command--queue-position (mod (1+ claude-command--queue-position) total))
      (let ((buffer-name (nth claude-command--queue-position entries)))
        (claude-command--switch-to-workspace-for-buffer buffer-name)
        (message "Queue position %d/%d: %s" (1+ claude-command--queue-position) total buffer-name)))))

;;;###autoload
(defun claude-command-queue-previous ()
  "Navigate to the previous entry in the taskmaster.org queue."
  (interactive)
  (claude-command--record-previous-workspace)
  (let* ((entries (claude-command--get-all-queue-entries))
         (total (length entries)))
    (if (zerop total)
        (message "No entries in queue")
      (setq claude-command--queue-position (mod (1- claude-command--queue-position) total))
      (let ((buffer-name (nth claude-command--queue-position entries)))
        (claude-command--switch-to-workspace-for-buffer buffer-name)
        (message "Queue position %d/%d: %s" (1+ claude-command--queue-position) total buffer-name)))))

;;;###autoload
(defun claude-command-queue-skip ()
  "Skip the current queue entry (delete it) and advance to the next."
  (interactive)
  (let* ((entries (claude-command--get-all-queue-entries))
         (total (length entries)))
    (if (zerop total)
        (message "No entries in queue")
      (let ((current-buffer (nth claude-command--queue-position entries)))
        (if (claude-command--delete-queue-entry-for-buffer current-buffer)
            (progn
              (message "Skipped entry for %s" current-buffer)
              ;; Adjust position if we're at the end
              (let ((new-total (length (claude-command--get-all-queue-entries))))
                (when (>= claude-command--queue-position new-total)
                  (setq claude-command--queue-position (max 0 (1- new-total))))
                (if (zerop new-total)
                    (message "Queue is now empty")
                  (claude-command-queue-next))))
          (message "Failed to skip entry for %s" current-buffer))))))

;;;###autoload
(defun claude-command-queue-status ()
  "Show the current queue status."
  (interactive)
  (let* ((entries (claude-command--get-all-queue-entries))
         (total (length entries)))
    (if (zerop total)
        (message "Queue is empty")
      (message "Queue: %d/%d entries, current: %s" 
               (1+ claude-command--queue-position) total 
               (nth claude-command--queue-position entries)))))

;;;###autoload
(defun claude-command-queue-browse ()
  "Browse and select from the taskmaster.org queue using minibuffer completion."
  (interactive)
  (claude-command--record-previous-workspace)
  (let ((entries (claude-command--get-all-queue-entries)))
    (if (null entries)
        (message "Queue is empty")
      (let* ((choices (cl-loop for entry in entries
                               for i from 0
                               collect (cons (format "%d. %s" (1+ i) entry) entry)))
             (selection (completing-read "Select queue entry: " choices nil t))
             (selected-buffer (cdr (assoc selection choices))))
        (when selected-buffer
          ;; Update queue position to match selection
          (setq claude-command--queue-position (cl-position selected-buffer entries :test #'string=))
          ;; Switch to the selected buffer
          (claude-command--switch-to-workspace-for-buffer selected-buffer)
          (message "Switched to queue entry: %s" selected-buffer))))))

;;;; Queue Cleanup on Buffer Kill

(defun claude-command--cleanup-queue-entries ()
  "Remove taskmaster.org entries when Claude buffer is killed.

This function is added to `kill-buffer-hook' in Claude buffers to automatically
clean up queue entries when the buffer is no longer available."
  (let ((buffer-name (buffer-name)))
    (when (and buffer-name (string-match-p "^\\*claude:" buffer-name))
      (claude-command--delete-queue-entry-for-buffer buffer-name))))

;;;; Automatic Entry Clearing on RET

(defun claude-command--auto-clear-on-ret ()
  "Auto-clear taskmaster.org entry when user sends input.

This function is added to the RET key in Claude buffers to provide
seamless queue progression."
  (let ((buffer-name (buffer-name)))
    (when (string-match-p "^\\*claude:" buffer-name)
      (when (claude-command--delete-queue-entry-for-buffer buffer-name)
        (message "Auto-cleared queue entry for %s" buffer-name)))))

(defun claude-command--auto-advance-to-next ()
  "Clear current buffer from queue and advance to the next queue entry.

This function clears the current Claude buffer from the task queue and
automatically switches to the next available queue entry. If no more
entries exist, it displays a message."
  (let ((buffer-name (buffer-name)))
    (when (and claude-command-auto-advance-queue 
               (string-match-p "^\\*claude:" buffer-name))
      ;; Clear current buffer from queue
      (when (claude-command--delete-queue-entry-for-buffer buffer-name)
        (message "Cleared queue entry for %s" buffer-name)
        ;; Get remaining entries after clearing current one
        (let* ((remaining-entries (claude-command--get-all-queue-entries))
               ;; Filter out the current buffer from remaining entries (in case it wasn't properly cleared)
               (other-entries (cl-remove-if (lambda (buf-name) 
                                              (string= buf-name buffer-name))
                                            remaining-entries)))
          (if other-entries
              (progn
                ;; Reset queue position to 0 and advance to first different entry
                (setq claude-command--queue-position 0)
                (let ((next-buffer (nth claude-command--queue-position other-entries)))
                  (claude-command--switch-to-workspace-for-buffer next-buffer)
                  (message "Auto-advanced to next queue entry: %s (%d remaining)" 
                           next-buffer (length other-entries))))
            (message "Queue is now empty - no more entries to process")))))))

(defun claude-command--setup-auto-clear-hook ()
  "Set up automatic entry clearing for Claude buffers.
This function is added to `claude-command-start-hook' to enable automatic
queue progression when users respond to Claude."
  (when (string-match-p "^\\*claude:" (buffer-name))
    ;; Use pre-command-hook to detect when user is about to send input
    ;; This works better with terminal emulators than trying to override RET
    (add-hook 'pre-command-hook #'claude-command--check-for-input nil t)))

(defun claude-command--check-for-input ()
  "Check if user is sending input and auto-clear queue entry.
This runs on pre-command-hook in Claude buffers."
  (when (string-match-p "^\\*claude:" (buffer-name))
    ;; Check if this is an input send command (not backspace or other editing)
    (when (or (eq this-command 'self-insert-command)
              (eq this-command 'newline)
              (eq this-command 'electric-newline-and-maybe-indent)
              (eq this-command 'comint-send-input)
              (eq this-command 'term-send-input)
              (eq this-command 'eshell-send-input)
              (eq this-command 'claude-command--vterm-send-return)
              (string-match-p "send-return\\|send-input" (symbol-name (or this-command 'unknown))))
      ;; If auto-advance mode is enabled, use the advance function, otherwise just clear
      (if claude-command-auto-advance-queue
          (claude-command--auto-advance-to-next)
        (claude-command--auto-clear-on-ret)))))

;; Add the hook to set up auto-clearing in Claude buffers
(add-hook 'claude-command-start-hook #'claude-command--setup-auto-clear-hook)

;;;###autoload
(defun claude-command-toggle-auto-advance-queue ()
  "Toggle auto-advance queue mode on or off.

When enabled, pressing enter in a Claude buffer will clear it from the
queue and automatically advance to the next queue entry."
  (interactive)
  (setq claude-command-auto-advance-queue (not claude-command-auto-advance-queue))
  (message "Claude Command auto-advance queue mode %s" 
           (if claude-command-auto-advance-queue "enabled" "disabled")))

;;;; Hook Integration Setup

;;;###autoload
(defun claude-command-setup ()
  "Set up org-mode notifications using the claude-code-event-hook system."
  (interactive)
  (add-hook 'claude-code-event-hook 'claude-command-org-notification-listener)
  (message "Claude Command org-mode notifications configured"))

;;;###autoload  
(defun claude-command-remove ()
  "Remove org-mode notification listener from claude-code-event-hook."
  (interactive)
  (remove-hook 'claude-code-event-hook 'claude-command-org-notification-listener)
  (message "Claude Command org-mode notifications removed"))

;;;; Integration

;; Configure display rule for notification buffer
(add-to-list 'display-buffer-alist
             '("^\\*Claude Command Notification\\*$"
               (display-buffer-in-side-window)
               (side . bottom)
               (window-height . 0.1)
               (select . nil)
               (quit-window . kill)))

;;;; Keybindings

;; Battlestation keybindings - C-c C-b prefix (original)
(global-set-key (kbd "C-c C-b b") 'claude-command-queue-browse)           ; Browse queue
(global-set-key (kbd "C-c C-b [") 'claude-command-queue-previous)         ; Previous in queue  
(global-set-key (kbd "C-c C-b ]") 'claude-command-queue-next)             ; Next in queue
(global-set-key (kbd "C-c C-b g") 'claude-command-goto-recent-workspace)  ; Go to recent
(global-set-key (kbd "C-c C-b r") 'claude-command-return-to-previous)     ; Return to previous
(global-set-key (kbd "C-c C-b s") 'claude-command-queue-skip)             ; Skip current entry
(global-set-key (kbd "C-c C-b t") 'claude-command-toggle-auto-advance-queue) ; Toggle auto-advance
(global-set-key (kbd "C-c C-b ?") 'claude-command-queue-status)           ; Show queue status

;; More ergonomic battlestation keybindings - C-c b prefix
(global-set-key (kbd "C-c b b") 'claude-command-queue-browse)             ; Browse queue
(global-set-key (kbd "C-c b [") 'claude-command-queue-previous)           ; Previous in queue  
(global-set-key (kbd "C-c b ]") 'claude-command-queue-next)               ; Next in queue
(global-set-key (kbd "C-c b g") 'claude-command-goto-recent-workspace)    ; Go to recent
(global-set-key (kbd "C-c b r") 'claude-command-return-to-previous)       ; Return to previous
(global-set-key (kbd "C-c b s") 'claude-command-queue-skip)               ; Skip current entry
(global-set-key (kbd "C-c b t") 'claude-command-toggle-auto-advance-queue) ; Toggle auto-advance
(global-set-key (kbd "C-c b ?") 'claude-command-queue-status)             ; Show queue status

;;;; Additional interactive commands

;;;###autoload
(defun claude-command-check-pre-tool-use-status ()
  "Check if PreToolUse hooks are properly configured."
  (interactive)
  (let* ((settings-file (expand-file-name "~/.claude/settings.json"))
         (has-settings (file-exists-p settings-file))
         (has-pretooluse-hook nil))
    (when has-settings
      (condition-case err
          (let* ((config (json-read-file settings-file))
                 (hooks (cdr (assq 'hooks config)))
                 (pretooluse (cdr (assq 'PreToolUse hooks))))
            (setq has-pretooluse-hook (not (null pretooluse))))
        (error nil)))
    (message "PreToolUse status: Settings file: %s, PreToolUse hook: %s"
             (if has-settings "Found" "Missing")
             (if has-pretooluse-hook "Configured" "Not configured"))))

;;;; Package initialization

;; Set up modeline notifications
(claude-command--setup-modeline)

(provide 'claude-command)

;;; claude-command.el ends here
