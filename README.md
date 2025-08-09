# Claude Code Org Notifications

Org-mode notification queue system for Claude Code - provides persistent task tracking with smart popup notifications and workspace integration.

## Features

- **Persistent Task Queue** - Track completed Claude tasks in `~/.claude/taskmaster.org`
- **Smart Notifications** - Popup notifications only when Claude buffer isn't visible
- **Workspace Navigation** - Integration with perspective.el for workspace-aware switching
- **Auto-Advance Mode** - Automatically progress through queue after responding
- **Queue Management** - Navigate, browse, and skip queue entries with ease

## Installation

```elisp
;; Load the package
(require 'claude-code-org-notifications)

;; Set up the org notifications listener
(claude-code-org-notifications-setup)
```

## Keybindings

Add these keybindings to your Emacs configuration:

```elisp
;; Queue navigation
(global-set-key (kbd "C-c n n") 'claude-code-queue-next)
(global-set-key (kbd "C-c n p") 'claude-code-queue-previous)
(global-set-key (kbd "C-c n s") 'claude-code-queue-skip)
(global-set-key (kbd "C-c n b") 'claude-code-queue-browse)
(global-set-key (kbd "C-c n q") 'claude-code-queue-status)

;; Workspace navigation
(global-set-key (kbd "C-c n g") 'claude-code-goto-recent-workspace)
(global-set-key (kbd "C-c n c") 'claude-code-goto-recent-workspace-and-clear)

;; Toggle auto-advance mode
(global-set-key (kbd "C-c n a") 'claude-code-toggle-auto-advance-queue)

;; Test notification system
(global-set-key (kbd "C-c n t") 'claude-code-test-notification)
```

## Commands

### Queue Navigation

- `claude-code-queue-next` - Navigate to next entry in queue
- `claude-code-queue-previous` - Navigate to previous entry in queue
- `claude-code-queue-skip` - Skip current entry and advance
- `claude-code-queue-browse` - Browse and select from queue using minibuffer
- `claude-code-queue-status` - Show current queue status

### Workspace Commands

- `claude-code-goto-recent-workspace` - Go to most recent workspace from queue
- `claude-code-goto-recent-workspace-and-clear` - Go to workspace and clear entry

### Configuration

- `claude-code-toggle-auto-advance-queue` - Toggle auto-advance mode
- `claude-code-test-notification` - Test the notification system

## Configuration

### Customization Options

```elisp
;; Path to the org file for storing task notifications
(setq claude-code-taskmaster-org-file "~/.claude/taskmaster.org")

;; Enable auto-advance queue mode
(setq claude-code-auto-advance-queue t)
```

### Auto-Advance Mode

When enabled, pressing Enter in a Claude buffer will:
1. Clear the current buffer from the task queue
2. Automatically switch to the next Claude buffer in the queue

This provides a streamlined workflow for processing multiple completed tasks.

## How It Works

1. **Task Completion Detection**: Listens to Claude Code events via `claude-code-event-hook`
2. **Org Entry Creation**: Creates TODO entries in `~/.claude/taskmaster.org` with timestamps
3. **Smart Notifications**: Shows popup only when Claude buffer isn't currently visible
4. **Queue Management**: Navigate through completed tasks using queue commands
5. **Auto-Clear**: Entries are automatically cleared when you respond in a Claude buffer

## Dependencies

- Emacs 30.0+
- claude-code 0.2.0+
- org-mode 9.0+
- perspective.el (optional, for workspace integration)

## License

Licensed under the same terms as claude-code.el.