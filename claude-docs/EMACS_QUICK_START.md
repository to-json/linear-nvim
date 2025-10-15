# Linear CLI for Emacs - Quick Start Guide

## Installation

### 1. Install the CLI
```bash
# If you have Go installed
go install # from the linear-cli repository

# Or verify it's already installed
which linear-cli
# Should show: /Users/jaesaxon/go/bin/linear-cli
```

### 2. Load in Emacs
Add to your `~/.emacs.d/init.el`:

```elisp
;; Load linear.el
(add-to-list 'load-path "/Users/jaesaxon/hc/linear-cli")
(require 'linear)

;; Optional: Add keybindings
(global-set-key (kbd "C-c l c") 'linear-create-issue)
(global-set-key (kbd "C-c l v") 'linear-view-issue)
(global-set-key (kbd "C-c l l") 'linear-list-project-issues)
(global-set-key (kbd "C-c l m") 'linear-add-minicomment)
(global-set-key (kbd "C-c l C") 'linear-add-comment)
(global-set-key (kbd "C-c l s") 'linear-change-state)
(global-set-key (kbd "C-c l a") 'linear-assign-issue)
(global-set-key (kbd "C-c l t") 'linear-take-issue)
(global-set-key (kbd "C-c l e") 'linear-export-issue)

;; Optional: Customize export directory
(setq linear-export-dir (expand-file-name "~/Documents/linear-exports"))
```

### 3. Reload Emacs
```
M-x eval-buffer RET
```
Or restart Emacs.

## Basic Usage

### Create an Issue
```
M-x linear-create-issue RET
```

1. Select team (e.g., "Engineering (ENG)")
2. Select project (or "(No project)")
3. Select template (or "(No template)")
4. Edit the buffer:
   ```markdown
   # LINEAR ISSUE
   # Team: Engineering (ENG)

   # PROJECT:
   My Project

   # TITLE:
   Fix the broken widget

   # DESCRIPTION:
   The widget on the dashboard is not rendering correctly.

   # PRIORITY: (0=None, 1=Urgent, 2=High, 3=Normal, 4=Low)
   2
   ```
5. Save with `C-x C-s` to create the issue
6. Buffer closes automatically on success

### View an Issue
```
M-x linear-view-issue RET ENG-123 RET
```

The issue opens in a markdown buffer. You can:
- Edit the description (between `<!-- DESCRIPTION START/END -->`)
- Edit your own comments (between `<!-- COMMENT ID: xxx START/END -->`)
- Save with `C-x C-s` to update
- Navigate to sub-issues by copying their IDs

### List Project Issues
```
M-x linear-list-project-issues RET
```

1. Select a project
2. View list of issues with states and assignees
3. Press `RET` on any issue to open it

### Add a Quick Comment
```
M-x linear-add-minicomment RET ENG-123 RET
Comment text: Fixed the widget by restarting the server RET
```

Or from an open issue buffer:
```
M-x linear-add-minicomment RET RET
(Detects issue ID automatically)
Comment text: Updated description RET
```

### Add a Full Comment
```
M-x linear-add-comment RET ENG-123 RET
```

Opens a markdown buffer for writing a longer comment.
Save with `C-x C-s` to submit.

### Change Issue State
```
M-x linear-change-state RET ENG-123 RET
```

1. Fetches workflow states
2. Select new state (e.g., "In Progress")
3. Issue updates automatically

### Assign Issue
```
M-x linear-assign-issue RET ENG-123 RET
```

1. Fetches team members
2. Select assignee
3. Issue updates automatically

### Take an Issue (Self-Assign)
```
M-x linear-take-issue RET ENG-123 RET
```

Assigns the issue to yourself.

### Export Issue
```
M-x linear-export-issue RET ENG-123 RET
```

Exports issue to `~/.linear/past-tickets/ENG-123.md` (clean format, no edit markers).

## Context-Aware Operations

Many commands can detect the issue ID automatically:

### From an Open Issue Buffer
When viewing an issue, you can run commands without specifying the ID:

```
M-x linear-add-minicomment RET RET
(Leave issue ID empty, uses current buffer's issue)
```

### From an Issue List Buffer
When viewing a project's issues, pressing `RET` opens the issue at cursor.

### Manual Entry
If no context is available, you'll be prompted:
```
Enter issue ID (e.g., ENG-123):
```

## Keyboard Shortcuts

With the suggested keybindings:

| Key       | Command                      | Description                |
|-----------|------------------------------|----------------------------|
| `C-c l c` | linear-create-issue          | Create new issue          |
| `C-c l v` | linear-view-issue            | View/edit issue           |
| `C-c l l` | linear-list-project-issues   | List project issues       |
| `C-c l m` | linear-add-minicomment       | Quick comment             |
| `C-c l C` | linear-add-comment           | Full comment (buffer)     |
| `C-c l s` | linear-change-state          | Change issue state        |
| `C-c l a` | linear-assign-issue          | Assign to team member     |
| `C-c l t` | linear-take-issue            | Take (self-assign)        |
| `C-c l e` | linear-export-issue          | Export to markdown        |

## Buffer Types

### Issue Creation Buffer
- **Name:** `*linear-create: TEAM-KEY/PROJECT*`
- **Mode:** markdown-mode
- **Save:** `C-x C-s` creates the issue
- **Cancel:** `C-x k` to kill buffer without saving

### Issue View Buffer
- **Name:** `*linear: ISSUE-ID*`
- **Mode:** markdown-mode
- **Save:** `C-x C-s` updates issue
- **Read-only regions:** Metadata and other users' comments
- **Editable regions:** Description and your comments

### Issue List Buffer
- **Name:** `*linear-issues: PROJECT-NAME*`
- **Mode:** special-mode (read-only)
- **Navigate:** `RET` opens issue at cursor
- **Quit:** `q` to quit

### Comment Buffer
- **Name:** `*linear-comment: ISSUE-ID*`
- **Mode:** markdown-mode
- **Save:** `C-x C-s` submits comment
- **Cancel:** `C-x k` to kill buffer without saving

## Customization

### Change Export Directory
```elisp
(setq linear-export-dir (expand-file-name "~/my-linear-exports"))
```

### Override CLI Path
```elisp
(setq linear-cli-path "/custom/path/to/linear-cli")
```

### Use Different Completion Framework
The package works with:
- Default Emacs completion
- Ivy (`counsel`)
- Helm
- Vertico
- Any other `completing-read` compatible framework

Just install your preferred framework and it will work automatically.

## Troubleshooting

### "Linear CLI not found"
```elisp
;; Check if CLI is found
M-x shell-command RET which linear-cli RET

;; If not found, ensure it's in GOPATH/bin
M-x shell-command RET go env GOPATH RET

;; Or set path manually
(setq linear-cli-path "/path/to/linear-cli")
```

### "Command failed: ..."
Check that you're authenticated with Linear:
```bash
linear-cli auth
```

### Buffer Won't Save
Make sure you're in the correct buffer and the content is valid:
- Issue creation: Title is required
- Issue update: Check for parse errors
- Comment: Comment text is required

### Completion Not Working
Install a completion framework for better UX:
```elisp
;; Option 1: Ivy
(use-package ivy
  :ensure t
  :config (ivy-mode 1))

;; Option 2: Vertico
(use-package vertico
  :ensure t
  :init (vertico-mode))
```

## Tips and Tricks

### 1. View Issue from Browser
Copy issue ID from Linear web app, then:
```
M-x linear-view-issue RET <paste-id> RET
```

### 2. Quick Status Updates
Open issue, change state, add comment, all from keyboard:
```
C-c l v RET ENG-123 RET    ; Open issue
C-c l s RET RET             ; Change state (uses context)
C-c l m RET RET             ; Add minicomment (uses context)
```

### 3. Export Before Closing
Before marking an issue as done:
```
C-c l e RET RET             ; Export (uses context)
C-c l s RET RET             ; Change to "Done"
```

### 4. Batch Operations
Use issue list to review multiple issues:
```
C-c l l RET                 ; List issues
RET                         ; Open first issue
C-x k RET                   ; Close buffer
RET                         ; Open next issue
```

### 5. Edit Multiple Issues
Can have multiple issue buffers open:
```
C-c l v RET ENG-123 RET
C-x 3                       ; Split window
C-c l v RET ENG-124 RET
```

Edit both, save both with `C-x C-s` in each window.

## Workflow Examples

### Bug Fix Workflow
1. `C-c l l` - List project issues
2. Find bug, press `RET` to open
3. `C-c l t` - Take the issue
4. `C-c l s` - Change state to "In Progress"
5. Fix the bug in your code
6. `C-c l m` - Add comment: "Fixed in commit abc123"
7. `C-c l s` - Change state to "Done"

### Feature Request Workflow
1. `C-c l c` - Create new issue
2. Fill in title and description
3. Set priority to 3 (Normal)
4. `C-x C-s` - Save/create
5. Get issue ID from output
6. `C-c l a` - Assign to team member

### Sprint Planning Workflow
1. `C-c l l` - List project issues
2. Review each issue (press `RET`)
3. Update descriptions/comments
4. Assign issues (`C-c l a`)
5. Set priorities
6. Change states as needed

## Advanced Configuration

### Auto-Open Issue Lists on Startup
```elisp
(defun my-linear-startup ()
  "Open key Linear project on startup."
  (interactive)
  (linear-list-project-issues))

;; Add to init hook if desired
;; (add-hook 'emacs-startup-hook 'my-linear-startup)
```

### Custom Keybinding Prefix
```elisp
;; Use a different prefix (e.g., C-c i for "issue")
(global-set-key (kbd "C-c i c") 'linear-create-issue)
(global-set-key (kbd "C-c i v") 'linear-view-issue)
;; etc.
```

### Integration with Projectile
```elisp
(defun my-linear-for-project ()
  "Open Linear issues for current projectile project."
  (interactive)
  (let ((project-name (projectile-project-name)))
    ;; This assumes your Linear project matches Projectile project
    (linear-list-project-issues)))

(define-key projectile-mode-map (kbd "C-c p L") 'my-linear-for-project)
```

## Support

For issues or questions:
- Check the Linear CLI documentation
- Review `/Users/jaesaxon/hc/linear-cli/LINEAR_EMACS_SUMMARY.md`
- Check the source code: `/Users/jaesaxon/hc/linear-cli/linear.el`

## Version Info

- **Package Version:** 1.0.0
- **Emacs Version Required:** 27.1+
- **Dependencies:** json.el (built-in), cl-lib (built-in)
- **Optional:** markdown-mode (for syntax highlighting)
