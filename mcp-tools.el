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



;;;; Claude Code Queue Management Tools

(claude-code-defmcp claude-control-queue-status ()
                    "Get the current status of the Claude task queue."
                    :mcp-description "Show current queue status including number of entries and current position"
                    :mcp-schema '()
                    (if (fboundp 'claude-code--get-all-queue-entries)
                        (let* ((entries (claude-code--get-all-queue-entries))
                               (total (length entries))
                               (position (if (boundp 'claude-code--queue-position)
                                             claude-code--queue-position
                                           0)))
                          (if (zerop total)
                              "Queue is empty"
                            (format "Queue status: %d entries total\nCurrent position: %d/%d\nCurrent buffer: %s\nAll entries: %s"
                                    total
                                    (1+ position) total
                                    (if (< position total) (nth position entries) "N/A")
                                    (string-join entries ", "))))
                      "Queue management functions not available (claude-code-org-notifications not loaded)"))

(claude-code-defmcp claude-control-queue-list ()
                    "List all entries in the Claude task queue."
                    :mcp-description "Get a detailed list of all queue entries with their buffer names"
                    :mcp-schema '()
                    (if (fboundp 'claude-code--get-all-queue-entries)
                        (let ((entries (claude-code--get-all-queue-entries)))
                          (if (null entries)
                              "Queue is empty"
                            (claude-code-mcp-ensure-output-directory)
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

(claude-code-defmcp claude-control-queue-next ()
                    "Navigate to the next entry in the task queue."
                    :mcp-description "Move to and switch to the next buffer in the queue"
                    :mcp-schema '()
                    (if (fboundp 'claude-code-queue-next)
                        (progn
                          (call-interactively 'claude-code-queue-next)
                          "Advanced to next queue entry")
                      "Queue navigation functions not available"))

(claude-code-defmcp claude-control-queue-previous ()
                    "Navigate to the previous entry in the task queue."
                    :mcp-description "Move to and switch to the previous buffer in the queue"
                    :mcp-schema '()
                    (if (fboundp 'claude-code-queue-previous)
                        (progn
                          (call-interactively 'claude-code-queue-previous)
                          "Moved to previous queue entry")
                      "Queue navigation functions not available"))

(claude-code-defmcp claude-control-queue-skip (buffer-name)
                    "Skip (delete) a specific entry from the task queue."
                    :mcp-description "Remove a specific buffer from the queue without processing it"
                    :mcp-schema '((buffer-name . ((or string nil) "Buffer name to skip (optional - uses current position if not provided)")))
                    (if (fboundp 'claude-code--delete-queue-entry-for-buffer)
                        (let ((target-buffer (or buffer-name
                                                 (and (fboundp 'claude-code--get-queue-entry-at-position)
                                                      (boundp 'claude-code--queue-position)
                                                      (claude-code--get-queue-entry-at-position claude-code--queue-position)))))
                          (if target-buffer
                              (if (claude-code--delete-queue-entry-for-buffer target-buffer)
                                  (format "Successfully skipped queue entry for buffer: %s" target-buffer)
                                (format "Failed to skip entry for buffer: %s (entry may not exist)" target-buffer))
                            "No buffer specified and no current queue position available"))
                      "Queue management functions not available"))

(claude-code-defmcp claude-control-queue-goto-recent ()
                    "Go to the most recent workspace from the task queue."
                    :mcp-description "Switch to the workspace containing the most recently added queue entry"
                    :mcp-schema '()
                    (if (fboundp 'claude-code-goto-recent-workspace)
                        (progn
                          (call-interactively 'claude-code-goto-recent-workspace)
                          "Switched to most recent workspace from queue")
                      "Queue workspace functions not available"))

(claude-code-defmcp claude-control-queue-goto-recent-and-clear ()
                    "Go to the most recent workspace and clear its queue entry."
                    :mcp-description "Switch to the most recent workspace and remove its entry from the queue"
                    :mcp-schema '()
                    (if (fboundp 'claude-code-goto-recent-workspace-and-clear)
                        (progn
                          (call-interactively 'claude-code-goto-recent-workspace-and-clear)
                          "Switched to recent workspace and cleared its queue entry")
                      "Queue workspace functions not available"))

(claude-code-defmcp claude-control-queue-browse ()
                    "Browse and select from the task queue interactively."
                    :mcp-description "Show all queue entries and allow selection of which one to switch to"
                    :mcp-schema '()
                    (if (fboundp 'claude-code-queue-browse)
                        (progn
                          (call-interactively 'claude-code-queue-browse)
                          "Opened queue browser for selection")
                      "Queue browsing functions not available"))

(claude-code-defmcp claude-control-queue-toggle-auto-advance ()
                    "Toggle auto-advance queue mode on or off."
                    :mcp-description "Enable/disable automatic advancement to next queue entry when sending input"
                    :mcp-schema '()
                    (if (fboundp 'claude-code-toggle-auto-advance-queue)
                        (progn
                          (call-interactively 'claude-code-toggle-auto-advance-queue)
                          (format "Auto-advance queue mode is now %s"
                                  (if (boundp 'claude-code-auto-advance-queue)
                                      (if claude-code-auto-advance-queue "enabled" "disabled")
                                    "unknown")))
                      "Queue auto-advance functions not available"))

(claude-code-defmcp claude-control-queue-clear-all ()
                    "Clear all entries from the task queue."
                    :mcp-description "Remove all TODO entries from the taskmaster.org file"
                    :mcp-schema '()
                    (if (and (fboundp 'claude-code--get-all-queue-entries)
                             (boundp 'claude-code-taskmaster-org-file))
                        (let ((entries (claude-code--get-all-queue-entries))
                              (cleared-count 0))
                          (dolist (buffer-name entries)
                            (when (and (fboundp 'claude-code--delete-queue-entry-for-buffer)
                                       (claude-code--delete-queue-entry-for-buffer buffer-name))
                              (setq cleared-count (1+ cleared-count))))
                          (format "Cleared %d entries from the task queue" cleared-count))
                      "Queue management functions not available"))



(provide 'mcp-tools)
;;; mcp-tools.el ends here
