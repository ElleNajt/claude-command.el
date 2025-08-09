;;; claude-code-org-notifications-keybindings.el --- Keybindings for Claude Code Org Notifications -*- lexical-binding: t; -*-

;;; Commentary:
;; Suggested keybindings for claude-code-org-notifications.
;; Load this file or copy these bindings to your Emacs configuration.

;;; Code:

(require 'claude-code-org-notifications)

;; Queue navigation commands
(global-set-key (kbd "C-c n n") 'claude-code-queue-next)
(global-set-key (kbd "C-c n p") 'claude-code-queue-previous)
(global-set-key (kbd "C-c n s") 'claude-code-queue-skip)
(global-set-key (kbd "C-c n b") 'claude-code-queue-browse)
(global-set-key (kbd "C-c n q") 'claude-code-queue-status)

;; Workspace navigation
(global-set-key (kbd "C-c n g") 'claude-code-goto-recent-workspace)
(global-set-key (kbd "C-c n c") 'claude-code-goto-recent-workspace-and-clear)

;; Configuration toggles
(global-set-key (kbd "C-c n a") 'claude-code-toggle-auto-advance-queue)

;; Testing and setup
(global-set-key (kbd "C-c n t") 'claude-code-test-notification)
(global-set-key (kbd "C-c n S") 'claude-code-org-notifications-setup)
(global-set-key (kbd "C-c n R") 'claude-code-org-notifications-remove)

(provide 'claude-code-org-notifications-keybindings)

;;; claude-code-org-notifications-keybindings.el ends here