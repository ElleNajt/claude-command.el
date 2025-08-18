# Claude Command

Emacs integration for Claude - provides persistent task tracking with clean notifications and workspace integration.

## Features

- **Persistent Task Queue** - Track completed Claude tasks in `~/.claude/taskmaster.org`
- **Clean Notifications** - Minibuffer messages by default, with options for popups or none
- **Silent Operations** - No "Wrote file" messages cluttering your echo area
- **Workspace Navigation** - Integration with perspective.el for workspace-aware switching
- **Auto-Advance Mode** - Automatically progress through queue after responding
- **Queue Management** - Navigate, browse, and skip queue entries with ease
- **Deduplication** - Automatic removal of duplicate queue entries

## Installation

```elisp
;; Load the package
(require 'claude-command)

;; The package auto-initializes when loaded
```

## Keybindings

The package includes two sets of keybindings for queue management:

### Ergonomic Keybindings (C-c b prefix)

```elisp
;; More ergonomic battlestation keybindings - fewer keystrokes
(global-set-key (kbd "C-c b b") 'claude-command-queue-browse)              ; Browse queue
(global-set-key (kbd "C-c b [") 'claude-command-queue-previous)            ; Previous in queue  
(global-set-key (kbd "C-c b ]") 'claude-command-queue-next)                ; Next in queue
(global-set-key (kbd "C-c b g") 'claude-command-goto-recent-workspace)     ; Go to recent workspace
(global-set-key (kbd "C-c b r") 'claude-command-return-to-previous)        ; Return to previous buffer
(global-set-key (kbd "C-c b s") 'claude-command-queue-skip)                ; Skip current entry
(global-set-key (kbd "C-c b t") 'claude-command-toggle-auto-advance-queue) ; Toggle auto-advance
(global-set-key (kbd "C-c b ?") 'claude-command-queue-status)              ; Show queue status
```

### Original Keybindings (C-c C-b prefix)

```elisp
;; Original battlestation keybindings
(global-set-key (kbd "C-c C-b b") 'claude-command-queue-browse)           
(global-set-key (kbd "C-c C-b [") 'claude-command-queue-previous)         
(global-set-key (kbd "C-c C-b ]") 'claude-command-queue-next)             
(global-set-key (kbd "C-c C-b g") 'claude-command-goto-recent-workspace)  
(global-set-key (kbd "C-c C-b r") 'claude-command-return-to-previous)     
(global-set-key (kbd "C-c C-b s") 'claude-command-queue-skip)             
(global-set-key (kbd "C-c C-b t") 'claude-command-toggle-auto-advance-queue)
(global-set-key (kbd "C-c C-b ?") 'claude-command-queue-status)           
```

## Commands

### Queue Navigation

- `claude-command-queue-next` (`C-c b ]`) - Navigate to next entry in queue
- `claude-command-queue-previous` (`C-c b [`) - Navigate to previous entry in queue
- `claude-command-queue-skip` (`C-c b s`) - Skip current entry and advance
- `claude-command-queue-browse` (`C-c b b`) - Browse and select from queue using minibuffer
- `claude-command-queue-status` (`C-c b ?`) - Show current queue status

### Workspace Commands

- `claude-command-goto-recent-workspace` (`C-c b g`) - Go to most recent workspace from queue
- `claude-command-return-to-previous` (`C-c b r`) - Return to previous buffer

### Configuration

- `claude-command-toggle-auto-advance-queue` (`C-c b t`) - Toggle auto-advance mode

## Configuration

### Customization Options

```elisp
;; Path to the org file for storing task notifications
(setq claude-command-taskmaster-org-file "~/.claude/taskmaster.org")

;; Notification style: 'message (default), 'popup, 'both, or 'none
(setq claude-command-notification-style 'message)

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
3. **Smart Notifications**: Shows messages in minibuffer only when Claude buffer isn't currently visible
4. **Queue Management**: Navigate through completed tasks using queue commands
5. **Auto-Clear**: Entries are automatically cleared when you respond in a Claude buffer
6. **Silent File Operations**: Uses silent write operations to avoid cluttering the echo area

## MCP Integration

This package includes MCP (Model Context Protocol) integration through the [emacs-mcp](https://github.com/ElleNajt/emacs-mcp) server, providing Claude with direct access to Emacs functionality.

## Dependencies

- Emacs 30.0+
- org-mode 9.0+
- perspective.el (optional, for workspace integration)
- [emacs-mcp](https://github.com/ElleNajt/emacs-mcp) (optional, for MCP tools)

## License

Licensed under the same terms as Emacs.