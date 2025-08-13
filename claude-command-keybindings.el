;;; claude-command-org-notifications-keybindings.el --- Improved keybindings for Claude Command -*- lexical-binding: t; -*-

;;; Commentary:
;; Improved keybinding scheme focused on the most common workflow:
;; 1. Go to most recent Claude notification
;; 2. Return to previous workspace
;; 3. Browse queue with minibuffer completion
;;
;; Two keybinding options are provided - choose one set.

;;; Code:

(require 'claude-command-org-notifications)

;;;; Option 1: Standard C-c prefix keybindings (more compatible)

;; Core workflow - the three most used commands
(global-set-key (kbd "C-c C-g") 'claude-command-goto-recent-workspace)  ; Go to latest
(global-set-key (kbd "C-c C-r") 'claude-command-return-to-previous)     ; Return to previous workspace
(global-set-key (kbd "C-c C-q") 'claude-command-select-queue-item)      ; Queue selector (minibuffer)

;; Additional queue operations
(global-set-key (kbd "C-c q n") 'claude-command-queue-next)         ; Next in queue
(global-set-key (kbd "C-c q p") 'claude-command-queue-previous)     ; Previous in queue
(global-set-key (kbd "C-c q s") 'claude-command-queue-skip)         ; Skip current
(global-set-key (kbd "C-c q b") 'claude-command-queue-browse)       ; Browse queue (alternative)
(global-set-key (kbd "C-c q q") 'claude-command-queue-status)       ; Query status

;; Workspace operations
(global-set-key (kbd "C-c w c") 'claude-command-goto-recent-workspace-and-clear) ; Clear and go

;;;; Option 2: Faster keybindings (uncomment to use)
;; These provide quicker access but may conflict with other modes

;; (global-set-key (kbd "C-;") 'claude-command-goto-recent-workspace)      ; Super fast access
;; (global-set-key (kbd "C-'") 'claude-command-return-to-previous)         ; Quick return
;; (global-set-key (kbd "C-,") 'claude-command-select-queue-item)          ; Quick queue browse

;; Alternative with Super/Hyper keys if available
;; (global-set-key (kbd "s-g") 'claude-command-goto-recent-workspace)
;; (global-set-key (kbd "s-r") 'claude-command-return-to-previous)
;; (global-set-key (kbd "s-q") 'claude-command-select-queue-item)

;; Function keys for zero-modifier access
;; (global-set-key (kbd "<f5>") 'claude-command-select-queue-item)
;; (global-set-key (kbd "<f6>") 'claude-command-goto-recent-workspace)
;; (global-set-key (kbd "<f7>") 'claude-command-return-to-previous)

(provide 'claude-command-org-notifications-keybindings-improved)

;;; claude-command-org-notifications-keybindings-improved.el ends here
