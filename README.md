# Claude Command Org Notifications

Org-mode notification queue system for Claude Command - provides persistent task tracking with smart popup notifications and workspace integration.

## Features

- **Persistent Task Queue** - Track completed Claude tasks in `~/.claude/taskmaster.org`
- **Smart Notifications** - Popup notifications only when Claude buffer isn't visible
- **Workspace Navigation** - Integration with perspective.el for workspace-aware switching
- **Auto-Advance Mode** - Automatically progress through queue after responding
- **Queue Management** - Navigate, browse, and skip queue entries with ease

## Installation

```elisp
;; Load the package
(require 'claude-command-org-notifications)

;; Set up the org notifications listener
(claude-command-org-notifications-setup)
```

## Keybindings

Add these keybindings to your Emacs configuration:

```elisp
;; Queue navigation
(global-set-key (kbd "C-c n n") 'claude-command-queue-next)
(global-set-key (kbd "C-c n p") 'claude-command-queue-previous)
(global-set-key (kbd "C-c n s") 'claude-command-queue-skip)
(global-set-key (kbd "C-c n b") 'claude-command-queue-browse)
(global-set-key (kbd "C-c n q") 'claude-command-queue-status)

;; Workspace navigation
(global-set-key (kbd "C-c n g") 'claude-command-goto-recent-workspace)
(global-set-key (kbd "C-c n c") 'claude-command-goto-recent-workspace-and-clear)

;; Toggle auto-advance mode
(global-set-key (kbd "C-c n a") 'claude-command-toggle-auto-advance-queue)

;; Test notification system
(global-set-key (kbd "C-c n t") 'claude-command-test-notification)
```

## Commands

### Queue Navigation

- `claude-command-queue-next` - Navigate to next entry in queue
- `claude-command-queue-previous` - Navigate to previous entry in queue
- `claude-command-queue-skip` - Skip current entry and advance
- `claude-command-queue-browse` - Browse and select from queue using minibuffer
- `claude-command-queue-status` - Show current queue status

### Workspace Commands

- `claude-command-goto-recent-workspace` - Go to most recent workspace from queue
- `claude-command-goto-recent-workspace-and-clear` - Go to workspace and clear entry

### Configuration

- `claude-command-toggle-auto-advance-queue` - Toggle auto-advance mode
- `claude-command-test-notification` - Test the notification system

## Configuration

### Customization Options

```elisp
;; Path to the org file for storing task notifications
(setq claude-command-taskmaster-org-file "~/.claude/taskmaster.org")

;; Enable auto-advance queue mode
(setq claude-command-auto-advance-queue t)
```

### Auto-Advance Mode

When enabled, pressing Enter in a Claude buffer will:
1. Clear the current buffer from the task queue
2. Automatically switch to the next Claude buffer in the queue

This provides a streamlined workflow for processing multiple completed tasks.

## How It Works

1. **Task Completion Detection**: Listens to Claude Command events via `claude-command-event-hook`
2. **Org Entry Creation**: Creates TODO entries in `~/.claude/taskmaster.org` with timestamps
3. **Smart Notifications**: Shows popup only when Claude buffer isn't currently visible
4. **Queue Management**: Navigate through completed tasks using queue commands
5. **Auto-Clear**: Entries are automatically cleared when you respond in a Claude buffer

## Dependencies

- Emacs 30.0+
- claude-command 0.2.0+
- org-mode 9.0+
- perspective.el (optional, for workspace integration)

## License

Licensed under the same terms as claude-command.el.