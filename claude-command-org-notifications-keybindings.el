;;; claude-command-org-notifications-keybindings.el --- Keybindings for Claude Command Org Notifications -*- lexical-binding: t; -*-

;;; Commentary:
;; Suggested keybindings for claude-command-org-notifications.
;; Load this file or copy these bindings to your Emacs configuration.

;;; Code:

(require 'claude-command-org-notifications)

;; Queue navigation commands
(global-set-key (kbd "C-c n n") 'claude-command-queue-next)
(global-set-key (kbd "C-c n p") 'claude-command-queue-previous)
(global-set-key (kbd "C-c n s") 'claude-command-queue-skip)
(global-set-key (kbd "C-c n b") 'claude-command-queue-browse)
(global-set-key (kbd "C-c n q") 'claude-command-queue-status)

;; Workspace navigation
(global-set-key (kbd "C-c n g") 'claude-command-goto-recent-workspace)
(global-set-key (kbd "C-c n c") 'claude-command-goto-recent-workspace-and-clear)

;; Configuration toggles
(global-set-key (kbd "C-c n a") 'claude-command-toggle-auto-advance-queue)

;; Testing and setup
(global-set-key (kbd "C-c n t") 'claude-command-test-notification)
(global-set-key (kbd "C-c n S") 'claude-command-org-notifications-setup)
(global-set-key (kbd "C-c n R") 'claude-command-org-notifications-remove)

(provide 'claude-command-org-notifications-keybindings)

;;; claude-command-org-notifications-keybindings.el ends here
