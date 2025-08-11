;;; mcp-tools.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Elle Najt
;;
;; Author: Elle Najt <elle@Elles-MBP>
;; Maintainer: Elle Najt <elle@Elles-MBP>
;; Created: August 07, 2025
;; Modified: August 07, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/elle/mcp-tools
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:



;;;; Claude Command Queue Management Tools

(claude-command-defmcp claude-control-queue-status ()
                    "Get the current status of the Claude task queue."
                    :mcp-description "Show current queue status including number of entries and current position"
                    :mcp-schema '()
                    (if (fboundp 'claude-command--get-all-queue-entries)
                        (let* ((entries (claude-command--get-all-queue-entries))
                               (total (length entries))
                               (position (if (boundp 'claude-command--queue-position)
                                             claude-command--queue-position
                                           0)))
                          (if (zerop total)
                              "Queue is empty"
                            (format "Queue status: %d entries total\nCurrent position: %d/%d\nCurrent buffer: %s\nAll entries: %s"
                                    total
                                    (1+ position) total
                                    (if (< position total) (nth position entries) "N/A")
                                    (string-join entries ", "))))
                      "Queue management functions not available (claude-command-org-notifications not loaded)"))

(claude-command-defmcp claude-control-queue-list ()
                    "List all entries in the Claude task queue."
                    :mcp-description "Get a detailed list of all queue entries with their buffer names"
                    :mcp-schema '()
                    (if (fboundp 'claude-command--get-all-queue-entries)
                        (let ((entries (claude-command--get-all-queue-entries)))
                          (if (null entries)
                              "Queue is empty"
                            (claude-command-mcp-ensure-output-directory)
                            (let ((output-file "/tmp/ClaudeWorkingFolder/queue_entries.txt")
                                  (content (format "Claude Task Queue (%d entries):\n%s\n\n%s"
                                                   (length entries)
                                                   (make-string 50 ?=)
                                                   (string-join
                                                    (cl-loop for entry in entries
                                                             for i from 1
                                                             collect (format "%d. %s" i entry))
                                                    "\n"))))
                              (write-region content nil output-file)
                              (format "Queue entries written to %s\n\nSummary:\n%s"
                                      output-file
                                      (string-join entries "\n")))))
                      "Queue management functions not available"))

(claude-command-defmcp claude-control-queue-next ()
                    "Navigate to the next entry in the task queue."
                    :mcp-description "Move to and switch to the next buffer in the queue"
                    :mcp-schema '()
                    (if (fboundp 'claude-command-queue-next)
                        (progn
                          (call-interactively 'claude-command-queue-next)
                          "Advanced to next queue entry")
                      "Queue navigation functions not available"))

(claude-command-defmcp claude-control-queue-previous ()
                    "Navigate to the previous entry in the task queue."
                    :mcp-description "Move to and switch to the previous buffer in the queue"
                    :mcp-schema '()
                    (if (fboundp 'claude-command-queue-previous)
                        (progn
                          (call-interactively 'claude-command-queue-previous)
                          "Moved to previous queue entry")
                      "Queue navigation functions not available"))

(claude-command-defmcp claude-control-queue-skip (buffer-name)
                    "Skip (delete) a specific entry from the task queue."
                    :mcp-description "Remove a specific buffer from the queue without processing it"
                    :mcp-schema '((buffer-name . ((or string nil) "Buffer name to skip (optional - uses current position if not provided)")))
                    (if (fboundp 'claude-command--delete-queue-entry-for-buffer)
                        (let ((target-buffer (or buffer-name
                                                 (and (fboundp 'claude-command--get-queue-entry-at-position)
                                                      (boundp 'claude-command--queue-position)
                                                      (claude-command--get-queue-entry-at-position claude-command--queue-position)))))
                          (if target-buffer
                              (if (claude-command--delete-queue-entry-for-buffer target-buffer)
                                  (format "Successfully skipped queue entry for buffer: %s" target-buffer)
                                (format "Failed to skip entry for buffer: %s (entry may not exist)" target-buffer))
                            "No buffer specified and no current queue position available"))
                      "Queue management functions not available"))

(claude-command-defmcp claude-control-queue-goto-recent ()
                    "Go to the most recent workspace from the task queue."
                    :mcp-description "Switch to the workspace containing the most recently added queue entry"
                    :mcp-schema '()
                    (if (fboundp 'claude-command-goto-recent-workspace)
                        (progn
                          (call-interactively 'claude-command-goto-recent-workspace)
                          "Switched to most recent workspace from queue")
                      "Queue workspace functions not available"))

(claude-command-defmcp claude-control-queue-goto-recent-and-clear ()
                    "Go to the most recent workspace and clear its queue entry."
                    :mcp-description "Switch to the most recent workspace and remove its entry from the queue"
                    :mcp-schema '()
                    (if (fboundp 'claude-command-goto-recent-workspace-and-clear)
                        (progn
                          (call-interactively 'claude-command-goto-recent-workspace-and-clear)
                          "Switched to recent workspace and cleared its queue entry")
                      "Queue workspace functions not available"))

(claude-command-defmcp claude-control-queue-browse ()
                    "Browse and select from the task queue interactively."
                    :mcp-description "Show all queue entries and allow selection of which one to switch to"
                    :mcp-schema '()
                    (if (fboundp 'claude-command-queue-browse)
                        (progn
                          (call-interactively 'claude-command-queue-browse)
                          "Opened queue browser for selection")
                      "Queue browsing functions not available"))

(claude-command-defmcp claude-control-queue-toggle-auto-advance ()
                    "Toggle auto-advance queue mode on or off."
                    :mcp-description "Enable/disable automatic advancement to next queue entry when sending input"
                    :mcp-schema '()
                    (if (fboundp 'claude-command-toggle-auto-advance-queue)
                        (progn
                          (call-interactively 'claude-command-toggle-auto-advance-queue)
                          (format "Auto-advance queue mode is now %s"
                                  (if (boundp 'claude-command-auto-advance-queue)
                                      (if claude-command-auto-advance-queue "enabled" "disabled")
                                    "unknown")))
                      "Queue auto-advance functions not available"))

(claude-command-defmcp claude-control-queue-clear-all ()
                    "Clear all entries from the task queue."
                    :mcp-description "Remove all TODO entries from the taskmaster.org file"
                    :mcp-schema '()
                    (if (and (fboundp 'claude-command--get-all-queue-entries)
                             (boundp 'claude-command-taskmaster-org-file))
                        (let ((entries (claude-command--get-all-queue-entries))
                              (cleared-count 0))
                          (dolist (buffer-name entries)
                            (when (and (fboundp 'claude-command--delete-queue-entry-for-buffer)
                                       (claude-command--delete-queue-entry-for-buffer buffer-name))
                              (setq cleared-count (1+ cleared-count))))
                          (format "Cleared %d entries from the task queue" cleared-count))
                      "Queue management functions not available"))

(claude-command-defmcp claude-control-get-all-buffers ()
                    "Get all Claude buffers currently running."
                    :mcp-description "List all active Claude buffers across all projects and directories"
                    :mcp-schema '()
                    (if (fboundp 'claude-command--find-all-claude-buffers)
                        (let ((buffers (claude-command--find-all-claude-buffers)))
                          (if (null buffers)
                              "No Claude buffers are currently active"
                            (claude-command-mcp-ensure-output-directory)
                            (let* ((buffer-names (mapcar #'buffer-name buffers))
                                   (output-file "/tmp/ClaudeWorkingFolder/claude_buffers.txt")
                                   (content (format "Active Claude Buffers (%d total):\n%s\n\nBuffer Details:\n%s"
                                                   (length buffer-names)
                                                   (string-join (cl-mapcar (lambda (i name)
                                                                             (format "%d. %s" i name))
                                                                           (number-sequence 1 (length buffer-names))
                                                                           buffer-names)
                                                               "\n")
                                                   (string-join
                                                    (mapcar (lambda (buffer)
                                                              (let* ((name (buffer-name buffer))
                                                                     (workspace (and (fboundp 'claude-command--find-workspace-for-buffer)
                                                                                    (claude-command--find-workspace-for-buffer name)))
                                                                     (directory (and (fboundp 'claude-command--extract-directory-from-buffer-name)
                                                                                    (claude-command--extract-directory-from-buffer-name name))))
                                                                (format "Buffer: %s\n  Workspace: %s\n  Directory: %s"
                                                                       name
                                                                       (or workspace "none")
                                                                       (or directory "unknown"))))
                                                            buffers)
                                                    "\n\n"))))
                              (with-temp-file output-file
                                (insert content))
                              (format "Found %d Claude buffers. Details written to %s"
                                     (length buffer-names)
                                     output-file))))
                      "Claude buffer functions not available (claude-command not loaded)"))



(claude-command-defmcp claude-control-queue-add-item (buffer-name message)
                    "Add a specific Claude buffer to the task queue."
                    :mcp-description "Add a Claude buffer to the queue with a custom message explaining why it needs attention"
                    :mcp-schema '((buffer-name . "string") (message . "string"))
                    (if (fboundp 'claude-command--add-org-todo-entry)
                        (progn
                          (claude-command--add-org-todo-entry buffer-name message)
                          (format "Added '%s' to queue with message: %s" buffer-name message))
                      "Queue management functions not available (claude-command-org-notifications not loaded)"))

(provide 'mcp-tools)
;;; mcp-tools.el ends here
