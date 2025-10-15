;;; linear.el --- Linear CLI integration for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Claude <noreply@anthropic.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (json "1.4"))
;; Keywords: tools, project-management, linear
;; URL: https://github.com/yourusername/linear-cli

;;; Commentary:

;; This package provides complete Linear CLI integration for Emacs, allowing
;; you to manage Linear issues directly from your editor.
;;
;; Features:
;; - Create issues with team/project/template selection
;; - View and edit issues (description and your own comments)
;; - List project issues with navigation
;; - Add comments (buffer-based or quick input)
;; - Change issue state
;; - Assign/unassign issues
;; - Export issues to markdown files
;;
;; Installation:
;; 1. Install the linear-cli binary from https://github.com/yourusername/linear-cli
;;    - Go to the repository and run `go install`
;;    - Or place binary at ~/hc/linear-cli/linear-cli
;;
;; 2. Add to your init.el:
;;    (require 'linear)
;;    (global-set-key (kbd "C-c l") 'linear-command-map)
;;
;; Usage:
;; - M-x linear-create-issue - Create a new issue
;; - M-x linear-view-issue - View/edit an issue
;; - M-x linear-list-project-issues - List issues for a project
;; - M-x linear-add-comment - Add comment to an issue
;; - M-x linear-change-state - Change issue state
;; - M-x linear-assign-issue - Assign issue to team member
;; - M-x linear-take-issue - Take (self-assign) an issue
;; - M-x linear-export-issue - Export issue to markdown

;;; Code:

(require 'json)
(require 'cl-lib)

;;; Customization

(defgroup linear nil
  "Linear CLI integration for Emacs."
  :group 'tools
  :prefix "linear-")

(defcustom linear-export-dir (expand-file-name "~/.linear/past-tickets")
  "Directory where exported Linear issues are saved."
  :type 'directory
  :group 'linear)

(defcustom linear-cli-path nil
  "Path to linear-cli binary.
If nil, will search GOPATH/bin and fallback locations."
  :type '(choice (const :tag "Auto-detect" nil)
                 (file :tag "Custom path"))
  :group 'linear)

;;; Internal Variables

(defvar linear--cli-path nil
  "Cached path to linear-cli binary.")

(defvar-local linear-issue-id nil
  "Issue ID for the current Linear issue buffer.")

(defvar-local linear-issue-data nil
  "Original issue data for the current Linear issue buffer.")

(defvar-local linear-issue-map nil
  "Hash table mapping line numbers to issue IDs in issue list buffers.")

(defvar-local linear-team nil
  "Team data for Linear issue creation buffer.")

(defvar-local linear-project nil
  "Project data for Linear issue creation buffer.")

(defvar-local linear-comment-issue-id nil
  "Issue ID for Linear comment buffer.")

;;; CLI Path Discovery

(defun linear--find-cli ()
  "Find the linear-cli binary path.
Checks custom path, GOPATH/bin, and fallback location."
  (or linear-cli-path
      (let* ((gopath (string-trim (shell-command-to-string "go env GOPATH")))
             (gopath-cli (expand-file-name "bin/linear-cli" gopath)))
        (cond
         ((and (file-exists-p gopath-cli)
               (file-executable-p gopath-cli))
          gopath-cli)
         ((file-executable-p (expand-file-name "~/hc/linear-cli/linear-cli"))
          (expand-file-name "~/hc/linear-cli/linear-cli"))
         (t nil)))))

(defun linear--ensure-cli ()
  "Ensure linear-cli is available, signal error if not."
  (unless linear--cli-path
    (setq linear--cli-path (linear--find-cli)))
  (unless (and linear--cli-path
               (file-exists-p linear--cli-path)
               (file-executable-p linear--cli-path))
    (user-error "Linear CLI not found. Please install from GOPATH or set linear-cli-path")))

;;; Process Execution

(defun linear--run-cli-sync (args)
  "Execute linear-cli synchronously with ARGS.
Returns (RESULT . ERROR) where RESULT is parsed JSON or nil,
and ERROR is error message string or nil."
  (linear--ensure-cli)
  (with-temp-buffer
    (let* ((exit-code (apply #'call-process
                            linear--cli-path nil t nil
                            (split-string args)))
           (output (buffer-string)))
      (if (zerop exit-code)
          (condition-case err
              (let ((json-object-type 'hash-table)
                    (json-array-type 'list)
                    (json-key-type 'string))
                (cons (json-read-from-string output) nil))
            (error (cons nil (format "Failed to parse JSON: %s" (error-message-string err)))))
        (cons nil (format "Command failed (exit %d): %s" exit-code output))))))

;;; JSON Helpers

(defun linear--hash-get (hash key &optional default)
  "Get KEY from HASH, return DEFAULT if not found."
  (if hash
      (gethash key hash default)
    default))

(defun linear--hash-get-nested (hash &rest keys)
  "Get nested value from HASH following KEYS path."
  (let ((value hash))
    (dolist (key keys)
      (setq value (if (hash-table-p value)
                      (gethash key value)
                    nil)))
    value))

;;; Issue ID Resolution

(defun linear--resolve-issue-id (&optional issue-id skip-buffer)
  "Resolve issue ID from multiple sources.
If ISSUE-ID is non-empty, return it.
Otherwise check buffer-local variables (unless SKIP-BUFFER).
Falls back to prompting user."
  (or (and issue-id (not (string-empty-p issue-id)) issue-id)
      (and (not skip-buffer)
           linear-issue-id
           (not (string-empty-p linear-issue-id))
           linear-issue-id)
      (and linear-issue-map
           (hash-table-p linear-issue-map)
           (gethash (line-number-at-pos) linear-issue-map))
      (read-string "Enter issue ID (e.g., ENG-123): ")))

;;; Selection Helper

(defun linear--select-from-list (prompt items format-fn)
  "Prompt user to select from ITEMS using PROMPT.
FORMAT-FN formats each item for display.
Returns selected item or nil if cancelled."
  (when items
    (let* ((choices (mapcar (lambda (item)
                             (cons (funcall format-fn item) item))
                           items))
           (selection (completing-read prompt choices nil t)))
      (cdr (assoc selection choices)))))

;;; Buffer Parsing

(defun linear--parse-issue-buffer ()
  "Parse issue creation buffer into structured data.
Returns alist with title, description, and priority."
  (let ((lines (split-string (buffer-string) "\n"))
        (issue '((title . "")
                (description . "")
                (priority . 0)))
        (current-section nil)
        (content-lines nil))
    (dolist (line lines)
      (cond
       ((string-match-p "^# TITLE:" line)
        (setq current-section 'title
              content-lines nil))
       ((string-match-p "^# DESCRIPTION:" line)
        (when (eq current-section 'title)
          (setf (alist-get 'title issue)
                (string-trim (mapconcat #'identity content-lines " "))))
        (setq current-section 'description
              content-lines nil))
       ((string-match-p "^# PRIORITY:" line)
        (when (eq current-section 'description)
          (setf (alist-get 'description issue)
                (string-trim (mapconcat #'identity content-lines "\n"))))
        (setq current-section 'priority
              content-lines nil))
       ((string-match-p "^# PROJECT:\\|^# TEAM:" line)
        (when (eq current-section 'priority)
          (setf (alist-get 'priority issue)
                (string-to-number (string-trim (mapconcat #'identity content-lines " ")))))
        (setq current-section nil))
       ((not (string-match-p "^#" line))
        (when (and current-section
                   (or (not (string-empty-p (string-trim line)))
                       content-lines))
          (push line content-lines)))))
    ;; Handle final section
    (cond
     ((eq current-section 'description)
      (setf (alist-get 'description issue)
            (string-trim (mapconcat #'identity (reverse content-lines) "\n"))))
     ((eq current-section 'priority)
      (setf (alist-get 'priority issue)
            (string-to-number (string-trim (mapconcat #'identity (reverse content-lines) " "))))))
    issue))

(defun linear--parse-editable-buffer ()
  "Parse issue view buffer to extract editable sections.
Returns alist with description and list of comments."
  (let ((lines (split-string (buffer-string) "\n" t))
        (result '((description . nil)
                 (comments . nil)))
        (current-section nil)
        (current-comment-id nil)
        (content-lines nil))
    (dolist (line lines)
      (cond
       ((string-match-p "^<!-- DESCRIPTION START -->" line)
        (setq current-section 'description
              content-lines nil))
       ((string-match-p "^<!-- DESCRIPTION END -->" line)
        (when (eq current-section 'description)
          (setf (alist-get 'description result)
                (mapconcat #'identity (reverse content-lines) "\n")))
        (setq current-section nil
              content-lines nil))
       ((string-match "^<!-- COMMENT ID: \\(.+\\) START -->" line)
        (setq current-section 'comment
              current-comment-id (match-string 1 line)
              content-lines nil))
       ((string-match-p "^<!-- COMMENT ID: .+ END -->" line)
        (when (eq current-section 'comment)
          (push (list (cons 'id current-comment-id)
                     (cons 'body (mapconcat #'identity (reverse content-lines) "\n")))
                (alist-get 'comments result)))
        (setq current-section nil
              current-comment-id nil
              content-lines nil))
       (current-section
        (let ((stripped-line (if (eq current-section 'comment)
                                (replace-regexp-in-string "^  " "" line)
                              line)))
          (push stripped-line content-lines)))))
    result))

;;; Markdown Mode Fallback

(unless (fboundp 'markdown-mode)
  (defun markdown-mode ()
    "Fallback markdown-mode if not installed."
    (text-mode)
    (font-lock-mode 1)))

;;; Issue Creation

(defun linear--create-issue-buffer (team project template)
  "Create issue creation buffer with TEAM, PROJECT, and TEMPLATE data."
  (let* ((project-name (if (and project (hash-table-p project))
                          (linear--hash-get project "name")
                        ""))
         (buf-name (format "*linear-create: %s%s*"
                          (linear--hash-get team "key")
                          (if (not (string-empty-p project-name))
                              (concat "/" (replace-regexp-in-string "[^[:alnum:]-]" "_" project-name))
                            "")))
         (buffer (generate-new-buffer buf-name)))
    (with-current-buffer buffer
      (insert "# LINEAR ISSUE\n")
      (insert (format "# Team: %s (%s)\n\n"
                     (linear--hash-get team "name")
                     (linear--hash-get team "key")))
      (insert "# PROJECT:\n")
      (if (and project (hash-table-p project))
          (insert (linear--hash-get project "name") "\n")
        (insert "(none)\n"))
      (insert "\n# TITLE:\n")
      (if (and template (hash-table-p template))
          (insert (or (linear--hash-get template "title") "") "\n")
        (insert "\n"))
      (insert "\n# DESCRIPTION:\n")
      (if (and template (hash-table-p template))
          (insert (or (linear--hash-get template "description") "") "\n")
        (insert "\n"))
      (insert "\n# PRIORITY: (0=None, 1=Urgent, 2=High, 3=Normal, 4=Low)\n")
      (if (and template (hash-table-p template))
          (insert (number-to-string (or (linear--hash-get template "priority") 0)) "\n")
        (insert "0\n"))
      (insert "\n# ---\n")
      (insert "# Save this buffer (:w or C-x C-s) to create the issue\n")
      (insert "# Close without saving to cancel\n")
      (markdown-mode)
      (setq linear-team team
            linear-project project)
      (add-hook 'write-contents-functions #'linear--submit-issue nil t)
      (goto-char (point-min))
      (search-forward "# TITLE:\n" nil t))
    buffer))

(defun linear--submit-issue ()
  "Submit issue from current buffer.
Returns t to indicate save was handled."
  (let* ((issue (linear--parse-issue-buffer))
         (title (alist-get 'title issue))
         (description (alist-get 'description issue))
         (priority (alist-get 'priority issue)))
    (if (string-empty-p title)
        (progn
          (message "Title is required")
          t)
      (linear--ensure-cli)
      (let ((cmd-args (list "create"
                           (format "--team=%s" (linear--hash-get linear-team "id"))
                           (format "--title=%s" title)
                           (format "--priority=%d" priority))))
        (when (not (string-empty-p description))
          (setq cmd-args (append cmd-args (list (format "--description=%s" description)))))
        (when (and linear-project (hash-table-p linear-project))
          (setq cmd-args (append cmd-args (list (format "--project=%s" (linear--hash-get linear-project "id"))))))
        (message "Creating issue...")
        (with-temp-buffer
          (let ((exit-code (apply #'call-process linear--cli-path nil t nil cmd-args)))
            (if (zerop exit-code)
                (progn
                  (message "Issue created successfully!")
                  (set-buffer-modified-p nil)
                  (kill-buffer)
                  t)
              (message "Failed to create issue: %s" (buffer-string))
              t)))))))

;;;###autoload
(defun linear-create-issue ()
  "Create a new Linear issue interactively.
Prompts for team, project, and template, then opens buffer for editing."
  (interactive)
  (message "Fetching teams...")
  (let ((teams-result (linear--run-cli-sync "teams --json")))
    (if (cdr teams-result)
        (message "Error fetching teams: %s" (cdr teams-result))
      (let* ((teams (car teams-result))
             (team (linear--select-from-list
                   "Select team: "
                   teams
                   (lambda (t-item)
                     (format "%s (%s)"
                            (linear--hash-get t-item "name")
                            (linear--hash-get t-item "key"))))))
        (if (not team)
            (message "Cancelled")
          (message "Fetching projects...")
          (let ((projects-result (linear--run-cli-sync "projects --json")))
            (if (cdr projects-result)
                (message "Error fetching projects: %s" (cdr projects-result))
              (let* ((projects (car projects-result))
                     (project-options (cons (let ((h (make-hash-table :test 'equal)))
                                             (puthash "id" nil h)
                                             (puthash "name" "(No project)" h)
                                             h)
                                           projects))
                     (project (linear--select-from-list
                              "Select project (optional): "
                              project-options
                              (lambda (p) (linear--hash-get p "name")))))
                (when (or (not project) (not (linear--hash-get project "id")))
                  (setq project nil))
                (message "Fetching templates...")
                (let ((templates-result (linear--run-cli-sync "templates --json")))
                  (if (cdr templates-result)
                      (message "Error fetching templates: %s" (cdr templates-result))
                    (let* ((all-templates (car templates-result))
                           (team-name (linear--hash-get team "name"))
                           (team-templates (if (hash-table-p all-templates)
                                             (linear--hash-get all-templates team-name)
                                           nil))
                           (template-options (cons (let ((h (make-hash-table :test 'equal)))
                                                    (puthash "id" nil h)
                                                    (puthash "name" "(No template)" h)
                                                    h)
                                                  (or team-templates '())))
                           (template (linear--select-from-list
                                     "Select template (optional): "
                                     template-options
                                     (lambda (tm) (linear--hash-get tm "name")))))
                      (when (or (not template) (not (linear--hash-get template "id")))
                        (setq template nil))
                      (let ((buffer (linear--create-issue-buffer team project template)))
                        (switch-to-buffer buffer)))))))))))))

;;;###autoload
(defalias 'linear-issue 'linear-create-issue
  "Alias for `linear-create-issue'.")

;;; Issue Viewing

(defun linear--format-issue-buffer (issue)
  "Format ISSUE data into lines for display buffer."
  (let ((lines '())
        (identifier (or (linear--hash-get issue "identifier") "???"))
        (title (or (linear--hash-get issue "title") "(no title)")))
    ;; Header
    (push (format "# %s: %s\n" identifier title) lines)
    (when (linear--hash-get issue "url")
      (push (format "**URL:** %s\n" (linear--hash-get issue "url")) lines))
    ;; Metadata
    (let* ((team (linear--hash-get issue "team"))
           (team-name (if (hash-table-p team)
                         (or (linear--hash-get team "name") "Unknown")
                       (or team "Unknown")))
           (state (linear--hash-get issue "state"))
           (state-name (if (hash-table-p state)
                          (or (linear--hash-get state "name") "Unknown")
                        (or state "Unknown")))
           (priority (or (linear--hash-get issue "priority") 0))
           (priority-labels '((0 . "None") (1 . "Urgent") (2 . "High") (3 . "Normal") (4 . "Low")))
           (priority-str (or (alist-get priority priority-labels) (number-to-string priority)))
           (assignee (linear--hash-get issue "assignee"))
           (assignee-name (if (hash-table-p assignee)
                            (or (linear--hash-get assignee "name") "Unassigned")
                          (or assignee "Unassigned"))))
      (push (format "**Team:** %s\n" team-name) lines)
      (push (format "**State:** %s\n" state-name) lines)
      (push (format "**Priority:** %s\n" priority-str) lines)
      (push (format "**Assignee:** %s\n" assignee-name) lines))
    ;; Parent issue
    (let ((parent (linear--hash-get issue "parent")))
      (when (hash-table-p parent)
        (push (format "**Parent Issue:** [%s] %s\n"
                     (or (linear--hash-get parent "identifier") "???")
                     (or (linear--hash-get parent "title") "(no title)"))
              lines)))
    (push "\n" lines)
    ;; Timestamps
    (when (linear--hash-get issue "createdAt")
      (push (format "**Created:** %s\n" (linear--hash-get issue "createdAt")) lines))
    (when (linear--hash-get issue "updatedAt")
      (push (format "**Updated:** %s\n" (linear--hash-get issue "updatedAt")) lines))
    (when (linear--hash-get issue "completedAt")
      (push (format "**Completed:** %s\n" (linear--hash-get issue "completedAt")) lines))
    (push "\n" lines)
    (push (make-string 80 ?-) lines)
    (push "\n\n## Description\n\n" lines)
    (push "<!-- DESCRIPTION START -->\n" lines)
    (let ((description (linear--hash-get issue "description")))
      (if (and description (not (string-empty-p description)))
          (push (concat description "\n") lines)
        (push "(no description)\n" lines)))
    (push "<!-- DESCRIPTION END -->\n\n" lines)
    (push (make-string 80 ?-) lines)
    (push "\n\n" lines)
    ;; Sub-issues
    (let* ((children (linear--hash-get issue "children"))
           (children-nodes (when (hash-table-p children)
                            (linear--hash-get children "nodes"))))
      (when children-nodes
        (push "## Sub-Issues\n\n" lines)
        (dolist (child children-nodes)
          (let ((child-id (or (linear--hash-get child "identifier") "???"))
                (child-title (or (linear--hash-get child "title") "(no title)"))
                (child-state (linear--hash-get child "state"))
                (child-state-name (if (hash-table-p child-state)
                                    (or (linear--hash-get child-state "name") "Unknown")
                                  "Unknown")))
            (push (format "- [%s] %s (%s)\n" child-id child-title child-state-name) lines)))
        (push "\n" lines)
        (push (make-string 80 ?-) lines)
        (push "\n\n" lines)))
    ;; Comments
    (push "## Comments\n\n" lines)
    (let* ((comments (linear--hash-get issue "comments"))
           (comments-nodes (cond
                           ((and (hash-table-p comments)
                                 (linear--hash-get comments "nodes"))
                            (linear--hash-get comments "nodes"))
                           ((listp comments) comments)
                           (t nil)))
           (current-user-name (or (linear--hash-get-nested issue "assignee" "name")
                                 (linear--hash-get-nested issue "creator" "name")))
           (i 1))
      (if comments-nodes
          (dolist (comment comments-nodes)
            (let* ((user (linear--hash-get comment "user"))
                   (author-name (if (hash-table-p user)
                                   (or (linear--hash-get user "name") "Unknown")
                                 (or user "Unknown")))
                   (timestamp (or (linear--hash-get comment "createdAt") "Unknown time"))
                   (is-user-comment (and current-user-name
                                        (string= author-name current-user-name)))
                   (comment-id (linear--hash-get comment "id"))
                   (body (linear--hash-get comment "body")))
              (push (format "### Comment %d\n" i) lines)
              (push (format "**%s** - %s\n\n" author-name timestamp) lines)
              (when (and is-user-comment comment-id)
                (push (format "<!-- COMMENT ID: %s START -->\n" comment-id) lines))
              (if (and body (not (string-empty-p body)))
                  (dolist (line (split-string body "\n" t))
                    (push (format "  %s\n" line) lines))
                (push "  (empty comment)\n" lines))
              (when (and is-user-comment comment-id)
                (push (format "<!-- COMMENT ID: %s END -->\n" comment-id) lines))
              (push "\n" lines)
              (setq i (1+ i))))
        (push "(no comments)\n" lines)))
    (apply #'concat (reverse lines))))

;;;###autoload
(defun linear-view-issue (issue-id)
  "View Linear issue ISSUE-ID in a buffer.
Issue can be edited and saved with C-x C-s."
  (interactive "sIssue ID (leave empty for context): ")
  (let ((resolved-id (linear--resolve-issue-id issue-id t)))
    (if (string-empty-p resolved-id)
        (message "No issue ID provided")
      (message "Fetching issue %s..." resolved-id)
      (let ((result (linear--run-cli-sync (format "get issue %s --json"
                                                  (shell-quote-argument resolved-id)))))
        (if (cdr result)
            (message "Error fetching issue: %s" (cdr result))
          (let* ((issue (car result))
                 (buf-name (format "*linear: %s*" resolved-id))
                 (existing-buffer (get-buffer buf-name))
                 (buffer (or existing-buffer (generate-new-buffer buf-name))))
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert (linear--format-issue-buffer issue))
                (markdown-mode)
                (setq linear-issue-id resolved-id
                      linear-issue-data issue)
                (add-hook 'write-contents-functions #'linear--save-issue-changes nil t)
                (goto-char (point-min))))
            (switch-to-buffer buffer)
            (message "Issue %s loaded (editable)" resolved-id)))))))

(defun linear--save-issue-changes ()
  "Save changes from issue view buffer.
Returns t to indicate save was handled."
  (let* ((parsed (linear--parse-editable-buffer))
         (parsed-desc (alist-get 'description parsed))
         (parsed-comments (alist-get 'comments parsed))
         (original-desc (linear--hash-get linear-issue-data "description"))
         (original-comments (linear--hash-get-nested linear-issue-data "comments" "nodes"))
         (updates '()))
    ;; Check description changes
    (when parsed-desc
      (let ((parsed-norm (string-trim parsed-desc))
            (original-norm (string-trim (or original-desc ""))))
        (unless (string= parsed-norm original-norm)
          (push (list (cons 'type 'description)
                     (cons 'text parsed-desc))
                updates))))
    ;; Check comment changes
    (dolist (parsed-comment parsed-comments)
      (let ((comment-id (alist-get 'id parsed-comment))
            (parsed-body (alist-get 'body parsed-comment))
            (original-body nil))
        (when original-comments
          (dolist (orig-comment original-comments)
            (when (string= (linear--hash-get orig-comment "id") comment-id)
              (setq original-body (linear--hash-get orig-comment "body")))))
        (let ((parsed-norm (string-trim parsed-body))
              (original-norm (string-trim (or original-body ""))))
          (when (and original-body (not (string= parsed-norm original-norm)))
            (push (list (cons 'type 'comment)
                       (cons 'id comment-id)
                       (cons 'text parsed-body))
                  updates)))))
    ;; Apply updates
    (if (null updates)
        (progn
          (message "No changes to save")
          (set-buffer-modified-p nil)
          t)
      (dolist (update updates)
        (let ((update-type (alist-get 'type update)))
          (cond
           ((eq update-type 'description)
            (message "Updating description...")
            (let* ((text (alist-get 'text update))
                   (cmd (format "update-description %s %s --json"
                               (shell-quote-argument linear-issue-id)
                               (shell-quote-argument text)))
                   (result (linear--run-cli-sync cmd)))
              (if (cdr result)
                  (message "Error updating description: %s" (cdr result))
                (message "Description updated"))))
           ((eq update-type 'comment)
            (message "Updating comment...")
            (let* ((comment-id (alist-get 'id update))
                   (text (alist-get 'text update))
                   (cmd (format "update-comment %s %s --json"
                               (shell-quote-argument comment-id)
                               (shell-quote-argument text)))
                   (result (linear--run-cli-sync cmd)))
              (if (cdr result)
                  (message "Error updating comment: %s" (cdr result))
                (message "Comment updated")))))))
      (message "All changes saved successfully")
      (linear-view-issue linear-issue-id)
      t)))

;;; Issue Listing

;;;###autoload
(defun linear-list-project-issues ()
  "List issues for a selected project."
  (interactive)
  (message "Fetching projects...")
  (let ((result (linear--run-cli-sync "projects --json")))
    (if (cdr result)
        (message "Error fetching projects: %s" (cdr result))
      (let* ((projects (car result))
             (project (linear--select-from-list
                      "Select project: "
                      projects
                      (lambda (p)
                        (let ((name (linear--hash-get p "name"))
                              (state (linear--hash-get p "state")))
                          (if state
                              (format "%s [%s]" name state)
                            name))))))
        (if (not project)
            (message "Cancelled")
          (message "Fetching issues for %s..." (linear--hash-get project "name"))
          (let ((issues-result (linear--run-cli-sync
                               (format "issues %s --json"
                                      (shell-quote-argument (linear--hash-get project "id"))))))
            (if (cdr issues-result)
                (message "Error fetching issues: %s" (cdr issues-result))
              (let* ((issues (car issues-result))
                     (project-name (linear--hash-get project "name"))
                     (buf-name (format "*linear-issues: %s*"
                                      (replace-regexp-in-string "[^[:alnum:]-]" "_" project-name)))
                     (buffer (generate-new-buffer buf-name))
                     (issue-map (make-hash-table :test 'equal)))
                (with-current-buffer buffer
                  (insert (format "PROJECT: %s\n" project-name))
                  (insert (format "%s\n\n" (make-string (+ 9 (length project-name)) ?=)))
                  (if (null issues)
                      (insert "No issues found for this project.\n")
                    (dolist (issue issues)
                      (let* ((identifier (or (linear--hash-get issue "identifier") "???"))
                             (title (or (linear--hash-get issue "title") "(no title)"))
                             (state (linear--hash-get issue "state"))
                             (state-name (if (hash-table-p state)
                                           (or (linear--hash-get state "name") "unknown")
                                         (or state "unknown")))
                             (assignee (linear--hash-get issue "assignee"))
                             (assignee-name (if (hash-table-p assignee)
                                              (or (linear--hash-get assignee "name") "unassigned")
                                            (or assignee "unassigned")))
                             (priority (or (linear--hash-get issue "priority") 0))
                             (priority-labels '((0 . "None") (1 . "Urgent") (2 . "High") (3 . "Normal") (4 . "Low")))
                             (priority-str (or (alist-get priority priority-labels) (number-to-string priority)))
                             (line-num (line-number-at-pos)))
                        (puthash line-num identifier issue-map)
                        (insert (format "%-10s  %s\n" identifier title))
                        (insert (format "            State: %s | Assignee: %s | Priority: %s\n\n"
                                       state-name assignee-name priority-str)))))
                  (insert "\nPress <RET> to open issue in new buffer\n")
                  (setq linear-issue-map issue-map)
                  (goto-char (point-min))
                  (special-mode)
                  (local-set-key (kbd "RET") 'linear--view-issue-from-map))
                (switch-to-buffer buffer)))))))))

(defun linear--view-issue-from-map ()
  "View issue at current line using issue map."
  (interactive)
  (when (and linear-issue-map (hash-table-p linear-issue-map))
    (let ((issue-id (gethash (line-number-at-pos) linear-issue-map)))
      (when issue-id
        (linear-view-issue issue-id)))))

;;; Comments

(defun linear--create-comment-buffer (issue-id)
  "Create comment buffer for ISSUE-ID."
  (let* ((buf-name (format "*linear-comment: %s*" issue-id))
         (buffer (generate-new-buffer buf-name)))
    (with-current-buffer buffer
      (insert "# LINEAR COMMENT\n")
      (insert (format "# Issue: %s\n\n" issue-id))
      (insert "# Write your comment below (markdown supported):\n\n\n")
      (insert "# ---\n")
      (insert "# Save this buffer (C-x C-s) to submit the comment\n")
      (insert "# Close without saving to cancel\n")
      (markdown-mode)
      (setq linear-comment-issue-id issue-id)
      (add-hook 'write-contents-functions #'linear--submit-comment nil t)
      (goto-char (point-min))
      (search-forward "# Write your comment below (markdown supported):\n\n" nil t))
    buffer))

(defun linear--submit-comment ()
  "Submit comment from current buffer.
Returns t to indicate save was handled."
  (let* ((lines (split-string (buffer-string) "\n"))
         (comment-lines '())
         (in-comment-section nil))
    (dolist (line lines)
      (cond
       ((string-match-p "^# Write your comment below" line)
        (setq in-comment-section t))
       ((string-match-p "^# ---" line)
        (setq in-comment-section nil))
       ((and in-comment-section (not (string-match-p "^#" line)))
        (push line comment-lines))))
    (let ((comment-text (string-trim (mapconcat #'identity (reverse comment-lines) "\n"))))
      (if (string-empty-p comment-text)
          (progn
            (message "Comment text is required")
            t)
        (message "Adding comment to %s..." linear-comment-issue-id)
        (let* ((cmd (format "comment %s %s --json"
                           (shell-quote-argument linear-comment-issue-id)
                           (shell-quote-argument comment-text)))
               (result (linear--run-cli-sync cmd)))
          (if (cdr result)
              (progn
                (message "Error adding comment: %s" (cdr result))
                t)
            (let ((comment-id (linear--hash-get (car result) "id")))
              (message "Comment added successfully (ID: %s)" (or comment-id "unknown")))
            (set-buffer-modified-p nil)
            (kill-buffer)
            ;; Refresh any open issue buffers
            (dolist (buf (buffer-list))
              (with-current-buffer buf
                (when (and linear-issue-id
                          (string= linear-issue-id linear-comment-issue-id))
                  (linear-view-issue linear-issue-id))))
            t))))))

;;;###autoload
(defun linear-add-comment (issue-id)
  "Add a comment to issue ISSUE-ID using buffer editor."
  (interactive "sIssue ID (leave empty for context): ")
  (let ((resolved-id (linear--resolve-issue-id issue-id)))
    (if (string-empty-p resolved-id)
        (message "No issue ID provided")
      (let ((buffer (linear--create-comment-buffer resolved-id)))
        (switch-to-buffer buffer)))))

;;;###autoload
(defun linear-add-minicomment (issue-id)
  "Add a quick comment to issue ISSUE-ID using minibuffer."
  (interactive "sIssue ID (leave empty for context): ")
  (let ((resolved-id (linear--resolve-issue-id issue-id)))
    (if (string-empty-p resolved-id)
        (message "No issue ID provided")
      (let ((comment-text (read-string "Comment text: ")))
        (if (string-empty-p comment-text)
            (message "Comment text is required")
          (message "Adding comment to %s..." resolved-id)
          (let* ((cmd (format "comment %s %s --json"
                             (shell-quote-argument resolved-id)
                             (shell-quote-argument comment-text)))
                 (result (linear--run-cli-sync cmd)))
            (if (cdr result)
                (message "Error adding comment: %s" (cdr result))
              (let ((comment-id (linear--hash-get (car result) "id")))
                (message "Comment added successfully (ID: %s)" (or comment-id "unknown")))
              ;; Refresh any open issue buffers
              (dolist (buf (buffer-list))
                (with-current-buffer buf
                  (when (and linear-issue-id (string= linear-issue-id resolved-id))
                    (linear-view-issue resolved-id)))))))))))

;;; State Management

;;;###autoload
(defun linear-change-state (issue-id)
  "Change state of issue ISSUE-ID."
  (interactive "sIssue ID (leave empty for context): ")
  (let ((resolved-id (linear--resolve-issue-id issue-id)))
    (if (string-empty-p resolved-id)
        (message "No issue ID provided")
      (message "Fetching issue %s..." resolved-id)
      (let ((issue-result (linear--run-cli-sync
                          (format "get issue %s --json"
                                 (shell-quote-argument resolved-id)))))
        (if (cdr issue-result)
            (message "Error fetching issue: %s" (cdr issue-result))
          (let* ((issue (car issue-result))
                 (team (linear--hash-get issue "team"))
                 (team-id (linear--hash-get team "id")))
            (if (not team-id)
                (message "Unable to determine team for issue")
              (message "Fetching workflow states for team %s..." (linear--hash-get team "name"))
              (let ((states-result (linear--run-cli-sync
                                   (format "states %s --json"
                                          (shell-quote-argument team-id)))))
                (if (cdr states-result)
                    (message "Error fetching workflow states: %s" (cdr states-result))
                  (let* ((states (car states-result))
                         (selected-state (linear--select-from-list
                                         "Select new state: "
                                         states
                                         (lambda (s)
                                           (format "%s (%s)"
                                                  (linear--hash-get s "name")
                                                  (linear--hash-get s "type"))))))
                    (if (not selected-state)
                        (message "Cancelled")
                      (message "Updating issue %s to state %s..."
                              resolved-id (linear--hash-get selected-state "name"))
                      (let* ((cmd (format "update-state %s %s --json"
                                         (shell-quote-argument resolved-id)
                                         (shell-quote-argument (linear--hash-get selected-state "id"))))
                             (result (linear--run-cli-sync cmd)))
                        (if (cdr result)
                            (message "Error updating issue state: %s" (cdr result))
                          (message "Issue state updated successfully to: %s"
                                  (linear--hash-get selected-state "name"))
                          ;; Refresh any open issue buffers
                          (dolist (buf (buffer-list))
                            (with-current-buffer buf
                              (when (and linear-issue-id (string= linear-issue-id resolved-id))
                                (linear-view-issue resolved-id)))))))))))))))))

;;; Assignment

;;;###autoload
(defun linear-assign-issue (issue-id)
  "Assign issue ISSUE-ID to a team member."
  (interactive "sIssue ID (leave empty for context): ")
  (let ((resolved-id (linear--resolve-issue-id issue-id)))
    (if (string-empty-p resolved-id)
        (message "No issue ID provided")
      (message "Fetching issue %s..." resolved-id)
      (let ((issue-result (linear--run-cli-sync
                          (format "get issue %s --json"
                                 (shell-quote-argument resolved-id)))))
        (if (cdr issue-result)
            (message "Error fetching issue: %s" (cdr issue-result))
          (let* ((issue (car issue-result))
                 (team (linear--hash-get issue "team"))
                 (team-id (linear--hash-get team "id")))
            (if (not team-id)
                (message "Unable to determine team for issue")
              (message "Fetching team members for %s..." (linear--hash-get team "name"))
              (let ((members-result (linear--run-cli-sync
                                    (format "team-members %s --json"
                                           (shell-quote-argument team-id)))))
                (if (cdr members-result)
                    (message "Error fetching team members: %s" (cdr members-result))
                  (let* ((members (car members-result))
                         (selected-member (linear--select-from-list
                                          "Select assignee: "
                                          members
                                          (lambda (m)
                                            (format "%s (%s)"
                                                   (linear--hash-get m "name")
                                                   (linear--hash-get m "email"))))))
                    (if (not selected-member)
                        (message "Cancelled")
                      (message "Assigning issue %s to %s..."
                              resolved-id (linear--hash-get selected-member "name"))
                      (let* ((cmd (format "assign %s %s --json"
                                         (shell-quote-argument resolved-id)
                                         (shell-quote-argument (linear--hash-get selected-member "id"))))
                             (result (linear--run-cli-sync cmd)))
                        (if (cdr result)
                            (message "Error assigning issue: %s" (cdr result))
                          (message "Issue assigned successfully to: %s"
                                  (linear--hash-get selected-member "name"))
                          ;; Refresh any open issue buffers
                          (dolist (buf (buffer-list))
                            (with-current-buffer buf
                              (when (and linear-issue-id (string= linear-issue-id resolved-id))
                                (linear-view-issue resolved-id)))))))))))))))))

;;;###autoload
(defun linear-unassign-issue (issue-id)
  "Unassign issue ISSUE-ID."
  (interactive "sIssue ID (leave empty for context): ")
  (let ((resolved-id (linear--resolve-issue-id issue-id)))
    (if (string-empty-p resolved-id)
        (message "No issue ID provided")
      (message "Unassigning issue %s..." resolved-id)
      (let* ((cmd (format "unassign %s --json" (shell-quote-argument resolved-id)))
             (result (linear--run-cli-sync cmd)))
        (if (cdr result)
            (message "Error unassigning issue: %s" (cdr result))
          (message "Issue unassigned successfully")
          ;; Refresh any open issue buffers
          (dolist (buf (buffer-list))
            (with-current-buffer buf
              (when (and linear-issue-id (string= linear-issue-id resolved-id))
                (linear-view-issue resolved-id)))))))))

;;;###autoload
(defun linear-take-issue (issue-id)
  "Take (self-assign) issue ISSUE-ID."
  (interactive "sIssue ID (leave empty for context): ")
  (let ((resolved-id (linear--resolve-issue-id issue-id)))
    (if (string-empty-p resolved-id)
        (message "No issue ID provided")
      (message "Taking issue %s..." resolved-id)
      (let* ((cmd (format "take %s --json" (shell-quote-argument resolved-id)))
             (result (linear--run-cli-sync cmd)))
        (if (cdr result)
            (message "Error taking issue: %s" (cdr result))
          (let* ((taken-issue (car result))
                 (assignee (linear--hash-get taken-issue "assignee"))
                 (assignee-name (if (hash-table-p assignee)
                                   (linear--hash-get assignee "name")
                                 "Unknown")))
            (message "Issue taken successfully, assigned to: %s" assignee-name)
            ;; Refresh any open issue buffers
            (dolist (buf (buffer-list))
              (with-current-buffer buf
                (when (and linear-issue-id (string= linear-issue-id resolved-id))
                  (linear-view-issue resolved-id))))))))))

;;; Export

;;;###autoload
(defun linear-export-issue (issue-id)
  "Export issue ISSUE-ID to markdown file."
  (interactive "sIssue ID (leave empty for context): ")
  (let ((resolved-id (linear--resolve-issue-id issue-id)))
    (if (string-empty-p resolved-id)
        (message "No issue ID provided")
      (message "Exporting issue %s..." resolved-id)
      (let ((result (linear--run-cli-sync
                    (format "get issue %s --json"
                           (shell-quote-argument resolved-id)))))
        (if (cdr result)
            (message "Error fetching issue: %s" (cdr result))
          (let* ((issue (car result))
                 (export-dir (expand-file-name linear-export-dir))
                 (filepath (expand-file-name (concat resolved-id ".md") export-dir)))
            (unless (file-directory-p export-dir)
              (make-directory export-dir t))
            (when (or (not (file-exists-p filepath))
                     (y-or-n-p (format "File %s exists. Overwrite? " filepath)))
              (linear--write-export-file issue resolved-id filepath))))))))

(defun linear--write-export-file (issue resolved-id filepath)
  "Write ISSUE data to FILEPATH for export.
RESOLVED-ID is the issue identifier."
  (let ((content (linear--format-export-content issue resolved-id)))
    (with-temp-file filepath
      (insert content))
    (message "Exported to: %s" filepath)
    (when (y-or-n-p "Open file in buffer? ")
      (find-file filepath))))

(defun linear--format-export-content (issue resolved-id)
  "Format ISSUE for export (no edit markers).
RESOLVED-ID is the issue identifier."
  (let ((lines '())
        (identifier (or (linear--hash-get issue "identifier") resolved-id))
        (title (or (linear--hash-get issue "title") "(no title)")))
    ;; Header
    (push (format "# [%s] %s\n\n" identifier title) lines)
    (when (linear--hash-get issue "url")
      (push (format "**URL:** %s\n" (linear--hash-get issue "url")) lines))
    ;; Metadata (similar to view but no edit markers)
    (let* ((team (linear--hash-get issue "team"))
           (team-name (if (hash-table-p team)
                         (or (linear--hash-get team "name") "Unknown")
                       (or team "Unknown")))
           (state (linear--hash-get issue "state"))
           (state-name (if (hash-table-p state)
                          (or (linear--hash-get state "name") "Unknown")
                        (or state "Unknown")))
           (priority (or (linear--hash-get issue "priority") 0))
           (priority-labels '((0 . "None") (1 . "Urgent") (2 . "High") (3 . "Normal") (4 . "Low")))
           (priority-str (or (alist-get priority priority-labels) (number-to-string priority)))
           (assignee (linear--hash-get issue "assignee"))
           (assignee-name (if (hash-table-p assignee)
                            (or (linear--hash-get assignee "name") "Unassigned")
                          (or assignee "Unassigned"))))
      (push (format "**Team:** %s\n" team-name) lines)
      (push (format "**State:** %s\n" state-name) lines)
      (push (format "**Priority:** %s\n" priority-str) lines)
      (push (format "**Assignee:** %s\n" assignee-name) lines))
    (let ((parent (linear--hash-get issue "parent")))
      (when (hash-table-p parent)
        (push (format "**Parent Issue:** [%s] %s\n"
                     (or (linear--hash-get parent "identifier") "???")
                     (or (linear--hash-get parent "title") "(no title)"))
              lines)))
    (when (linear--hash-get issue "createdAt")
      (push (format "**Created:** %s\n" (linear--hash-get issue "createdAt")) lines))
    (when (linear--hash-get issue "updatedAt")
      (push (format "**Updated:** %s\n" (linear--hash-get issue "updatedAt")) lines))
    (when (linear--hash-get issue "completedAt")
      (push (format "**Completed:** %s\n" (linear--hash-get issue "completedAt")) lines))
    (push "\n## Description\n\n" lines)
    (let ((description (linear--hash-get issue "description")))
      (if (and description (not (string-empty-p description)))
          (push (concat description "\n\n") lines)
        (push "(no description)\n\n" lines)))
    ;; Sub-issues
    (let* ((children (linear--hash-get issue "children"))
           (children-nodes (when (hash-table-p children)
                            (linear--hash-get children "nodes"))))
      (when children-nodes
        (push "## Sub-Issues\n\n" lines)
        (dolist (child children-nodes)
          (let ((child-id (or (linear--hash-get child "identifier") "???"))
                (child-title (or (linear--hash-get child "title") "(no title)"))
                (child-state (linear--hash-get child "state"))
                (child-state-name (if (hash-table-p child-state)
                                    (or (linear--hash-get child-state "name") "Unknown")
                                  "Unknown")))
            (push (format "- [%s] %s (%s)\n" child-id child-title child-state-name) lines)))
        (push "\n" lines)))
    ;; Comments (ALL comments, no edit markers)
    (push "## Comments\n\n" lines)
    (let* ((comments (linear--hash-get issue "comments"))
           (comments-nodes (cond
                           ((and (hash-table-p comments)
                                 (linear--hash-get comments "nodes"))
                            (linear--hash-get comments "nodes"))
                           ((listp comments) comments)
                           (t nil))))
      (if comments-nodes
          (dolist (comment comments-nodes)
            (let* ((user (linear--hash-get comment "user"))
                   (author-name (if (hash-table-p user)
                                   (or (linear--hash-get user "name") "Unknown")
                                 (or user "Unknown")))
                   (timestamp (or (linear--hash-get comment "createdAt") "Unknown time"))
                   (body (linear--hash-get comment "body")))
              (push (format "### Comment by %s - %s\n\n" author-name timestamp) lines)
              (if (and body (not (string-empty-p body)))
                  (push (concat body "\n\n") lines)
                (push "(empty comment)\n\n" lines))))
        (push "(no comments)\n" lines)))
    (apply #'concat (reverse lines))))

;;; Package Footer

(provide 'linear)

;;; linear.el ends here
