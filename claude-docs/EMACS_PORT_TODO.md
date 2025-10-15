# Comprehensive Emacs Lisp Porting Todo List for Linear CLI Plugin

**File:** `/Users/jaesaxon/hc/linear-cli/linear.el`

---

## 1. CORE INFRASTRUCTURE

### 1.1 Package Header and Dependencies
- [ ] Add proper Emacs package header with `;;; linear.el --- Linear CLI integration for Emacs`
- [ ] Define package metadata (`Author`, `Version`, `Package-Requires`, `Keywords`, `URL`)
- [ ] Add `(require 'json)` for JSON parsing (built-in since Emacs 23.1)
- [ ] Add `(require 'cl-lib)` for Common Lisp compatibility functions
- [ ] Consider adding `(require 'seq)` for sequence manipulation functions
- [ ] Define customization group: `(defgroup linear nil "Linear CLI integration" :group 'tools)`

### 1.2 Configuration Variables
- [ ] Create `defcustom linear-export-dir` with default `~/.linear/past-tickets`
- [ ] Use `(expand-file-name "~/.linear/past-tickets")` instead of string concatenation
- [ ] Document the customization variable properly with `:type 'directory`

### 1.3 CLI Path Discovery
- [ ] Implement `linear--find-cli` function:
  - Use `(call-process "go" nil t nil "env" "GOPATH")` to get GOPATH
  - Check `executable-find` for `gopath/bin/linear-cli`
  - Fall back to `~/hc/linear-cli/linear-cli`
  - Use `(expand-file-name)` for all path operations
- [ ] Store result in `defvar linear--cli-path` (double-dash for internal variable)
- [ ] Add error handling if CLI not found anywhere

### 1.4 Process Execution (Critical Decision Point)
**Decision: Use SYNCHRONOUS `call-process` initially (like Vim9), async later**

- [ ] Create `linear--run-cli-sync (args)` function:
  - Use `(with-temp-buffer ...)` to capture output
  - Use `(call-process linear--cli-path nil t nil ...)` where args are split
  - Parse exit code from return value
  - Return cons cell `(result . error)` or use `(list result error)`
  - Handle shell escaping with `shell-quote-argument`

**Future async consideration:**
- [ ] Create `linear--run-cli-async (args callback)` using `make-process`:
  - Set `:name`, `:buffer`, `:command`, `:sentinel`, `:filter`
  - Accumulate output in process buffer
  - Call callback in sentinel when process exits
  - Use `(set-process-sentinel)` for completion handling

### 1.5 JSON Parsing
- [ ] Create `linear--parse-json (output)` function:
  - Use `(condition-case err (json-read-from-string output) (error ...))`
  - Set `json-object-type` to `'hash-table` or `'alist` (decide which)
  - Set `json-array-type` to `'list`
  - Set `json-key-type` to `'string`
  - Return `(cons nil error-message)` on parse failure
  - Return `(cons parsed-result nil)` on success

---

## 2. ISSUE ID RESOLUTION

### 2.1 Buffer-Local Variables Strategy
- [ ] Use `defvar-local linear-issue-id nil` for issue view buffers
- [ ] Use `defvar-local linear-issue-map nil` for issue list buffers (hash-table mapping line numbers to issue IDs)
- [ ] Use `defvar-local linear-issue-data nil` for storing original issue data
- [ ] Use `defvar-local linear-team nil` for create buffers
- [ ] Use `defvar-local linear-project nil` for create buffers
- [ ] Use `defvar-local linear-comment-issue-id nil` for comment buffers

### 2.2 Issue ID Resolution Function
- [ ] Create `linear--resolve-issue-id (&optional issue-id skip-buffer)` function:
  - Check explicit `issue-id` argument first (if non-empty string)
  - If not `skip-buffer`, check `linear-issue-id` buffer-local variable
  - Check `linear-issue-map` with `(line-number-at-pos)` as key
  - Fall back to `(read-string "Enter issue ID (e.g., ENG-123): ")`
  - Return resolved issue ID string or nil if user cancels

---

## 3. BUFFER MANAGEMENT

### 3.1 Issue Creation Buffer

- [ ] Create `linear--create-issue-buffer (team project template)` function:
  - Generate buffer name: `*linear-create: TEAM-KEY/PROJECT-NAME*`
  - Use `(generate-new-buffer-name)` to avoid conflicts
  - Use `(get-buffer-create)` to create buffer
  - Set `buffer-file-name` to nil but `buffer-offer-save` to t
  - Set major mode to `markdown-mode` if available, else `text-mode`
  - Insert template content (header, PROJECT, TITLE, DESCRIPTION, PRIORITY sections)
  - Set buffer-local variables: `linear-team`, `linear-project`
  - Add buffer-local `write-contents-functions` hook: `(add-hook 'write-contents-functions #'linear--submit-issue nil t)`
  - Return buffer object

- [ ] Create `linear--parse-issue-buffer (buffer)` function:
  - Use `(with-current-buffer buffer ...)` to read lines
  - Parse sections: TITLE, DESCRIPTION, PRIORITY
  - Use regex matching `^# TITLE:`, `^# DESCRIPTION:`, etc.
  - Return alist: `((title . "...") (description . "...") (priority . 0))`

- [ ] Create `linear--submit-issue ()` function:
  - Parse current buffer with `linear--parse-issue-buffer`
  - Validate title is non-empty
  - Build CLI command with `shell-quote-argument` for all args
  - Execute synchronously with `call-process`
  - On success: display message, set `buffer-modified-p` to nil, kill buffer
  - On failure: display error, keep buffer open
  - Return t (consumed the save) or nil

### 3.2 Issue View Buffer (Editable)

- [ ] Create `linear--view-issue (issue-id)` function:
  - Resolve issue ID (with `skip-buffer` = t to avoid re-opening same issue)
  - Fetch issue data via CLI
  - Generate buffer name: `*linear: ISSUE-ID*`
  - Check if buffer already exists, reuse if so
  - Clear buffer, set to `markdown-mode`
  - Insert formatted issue data (header, metadata, description, sub-issues, comments)
  - Add HTML-style edit markers: `<!-- DESCRIPTION START -->`, `<!-- COMMENT ID: xxx START -->`
  - Only mark user's own comments as editable (match by author name)
  - Indent comment bodies with 2 spaces
  - Set buffer-local variables: `linear-issue-id`, `linear-issue-data`
  - Add `write-contents-functions` hook: `linear--save-issue-changes`
  - Make buffer read-write but associate with "file" for save
  - Position cursor at top
  - Display message

- [ ] Create `linear--parse-editable-buffer (buffer)` function:
  - Extract content between `<!-- DESCRIPTION START/END -->`
  - Extract content between `<!-- COMMENT ID: xxx START/END -->` markers
  - Strip 2-space indentation from comment bodies
  - Return alist: `((description . "...") (comments . (((id . "...") (body . "...")) ...)))`

- [ ] Create `linear--save-issue-changes ()` function:
  - Parse current buffer
  - Compare with `linear-issue-data` buffer-local variable
  - Normalize whitespace (trim) before comparison
  - Build list of updates: `((type . "description") (text . "..."))` or `((type . "comment") (id . "...") (text . "..."))`
  - If no changes, message "No changes to save", return t
  - Sequentially apply updates via CLI (`update-description`, `update-comment`)
  - On completion, refresh buffer by calling `linear--view-issue` again
  - Return t

### 3.3 Issue List Buffer

- [ ] Create `linear--list-project-issues ()` function:
  - Fetch projects via CLI
  - Prompt user with `completing-read` to select project
  - Fetch issues for selected project
  - Generate buffer name: `*linear-issues: PROJECT-NAME*`
  - Clear buffer, insert formatted issue list
  - Build `linear-issue-map` hash-table: `(line-number . issue-id)`
  - Set as buffer-local variable
  - Set up keymap: bind `RET` to `linear--view-issue-from-map`
  - Make buffer read-only
  - Display buffer

- [ ] Create `linear--view-issue-from-map ()` function:
  - Get current line number with `(line-number-at-pos)`
  - Lookup in `linear-issue-map` hash-table
  - Call `linear--view-issue` with found issue ID

### 3.4 Comment Buffer

- [ ] Create `linear--create-comment-buffer (issue-id)` function:
  - Generate buffer name: `*linear-comment: ISSUE-ID*`
  - Create buffer, set to `markdown-mode`
  - Insert header, instructions, and template
  - Set buffer-local variable: `linear-comment-issue-id`
  - Add `write-contents-functions` hook: `linear--submit-comment`
  - Return buffer

- [ ] Create `linear--submit-comment ()` function:
  - Parse buffer (skip lines starting with `#`, extract comment text after instruction header)
  - Validate non-empty comment
  - Submit via CLI `comment ISSUE-ID TEXT`
  - On success: kill buffer, refresh any open issue view buffers for this issue
  - Iterate through all buffers with `(buffer-list)`, check `linear-issue-id`, refresh if matches
  - Return t

### 3.5 Export Buffer

- [ ] Create `linear--export-issue (issue-id)` function:
  - Resolve issue ID
  - Fetch issue data
  - Build filepath: `(expand-file-name (concat issue-id ".md") linear-export-dir)`
  - Create directory if needed: `(make-directory linear-export-dir t)`
  - Check if file exists, prompt for overwrite with `y-or-n-p`
  - Call `linear--write-export-file`

- [ ] Create `linear--write-export-file (issue resolved-id filepath)` function:
  - Build markdown content (no edit markers, all comments included)
  - Write to file with `(with-temp-file filepath (insert ...))`
  - Display message with filepath
  - Prompt to open file: `(when (y-or-n-p "Open file?") (find-file filepath))`

---

## 4. INTERACTIVE COMMANDS

### 4.1 Issue Creation
- [ ] Define `(defun linear-create-issue ()`  with `(interactive)`:
  - Fetch teams, prompt with `completing-read`
  - Use `(mapcar (lambda (team) (cons (format "%s (%s)" name key) team)) teams)` for display
  - Fetch projects, add "(No project)" option, prompt
  - Fetch templates for selected team, add "(No template)" option, prompt
  - Call `linear--create-issue-buffer`
  - Display buffer with `(switch-to-buffer)`
  - Position cursor at title line: `(goto-char (point-min)) (search-forward "# TITLE:") (forward-line)`

- [ ] Define `(defalias 'linear-issue 'linear-create-issue)` for compatibility

### 4.2 Issue Viewing
- [ ] Define `(defun linear-view-issue (issue-id)` with `(interactive "sIssue ID: ")`:
  - Call `linear--view-issue` with resolved ID
  - Handle optional argument (use prefix for prompt override)

### 4.3 Issue Listing
- [ ] Define `(defun linear-list-project-issues ()` with `(interactive)`:
  - Call `linear--list-project-issues`

### 4.4 Comments
- [ ] Define `(defun linear-add-comment (issue-id)` with `(interactive "sIssue ID (leave empty for context): ")`:
  - Resolve issue ID (with context awareness)
  - Call `linear--create-comment-buffer`
  - Display buffer

- [ ] Define `(defun linear-add-minicomment (issue-id)` with similar `(interactive)`:
  - Resolve issue ID
  - Prompt for comment text with `(read-string "Comment text: ")`
  - Submit via CLI
  - Refresh any open issue buffers

### 4.5 State Management
- [ ] Define `(defun linear-change-state (issue-id)` with `(interactive "sIssue ID: ")`:
  - Resolve issue ID
  - Fetch issue to get team
  - Fetch workflow states for team
  - Prompt with `completing-read` for state selection
  - Submit via CLI `update-state ISSUE-ID STATE-ID`
  - Refresh any open issue buffers

### 4.6 Assignment
- [ ] Define `(defun linear-assign-issue (issue-id)` with `(interactive "sIssue ID: ")`:
  - Resolve issue ID
  - Fetch issue to get team
  - Fetch team members
  - Prompt with `completing-read` for member selection
  - Submit via CLI `assign ISSUE-ID MEMBER-ID`
  - Refresh any open issue buffers

- [ ] Define `(defun linear-unassign-issue (issue-id)` with `(interactive "sIssue ID: ")`:
  - Resolve issue ID
  - Submit via CLI `unassign ISSUE-ID`
  - Refresh any open issue buffers

- [ ] Define `(defun linear-take-issue (issue-id)` with `(interactive "sIssue ID: ")`:
  - Resolve issue ID
  - Submit via CLI `take ISSUE-ID`
  - Display assignee name from result
  - Refresh any open issue buffers

### 4.7 Export
- [ ] Define `(defun linear-export-issue (issue-id)` with `(interactive "sIssue ID: ")`:
  - Call `linear--export-issue`

---

## 5. EMACS-SPECIFIC CONSIDERATIONS

### 5.1 Major Mode vs Minor Mode Decision
**Recommendation: Create a minor mode for Linear integration**

- [ ] Define `(define-minor-mode linear-mode ...)`:
  - Provides buffer-local keymap for Linear commands
  - Lighter: `" Linear"`
  - Keymap with suggested bindings (user-configurable)
  - Don't enable by default, let user enable in hooks

### 5.2 Special Buffer Major Modes
**Create dedicated major modes for special buffers:**

- [ ] Define `(define-derived-mode linear-issue-list-mode special-mode "Linear-Issues" ...)`:
  - Inherits from `special-mode` (read-only buffer mode)
  - Define keymap: `RET` -> `linear--view-issue-from-map`
  - Set `buffer-read-only` to t
  - Disable line wrapping: `(setq truncate-lines t)`

- [ ] Define `(define-derived-mode linear-issue-view-mode markdown-mode "Linear-Issue" ...)`:
  - Inherits from `markdown-mode` if available, else `text-mode`
  - Define keymap with useful bindings (refresh, comment, change state, etc.)
  - Enable `visual-line-mode` for better markdown reading

- [ ] Define `(define-derived-mode linear-create-mode markdown-mode "Linear-Create" ...)`:
  - Similar to view mode but for issue creation

- [ ] Define `(define-derived-mode linear-comment-mode markdown-mode "Linear-Comment" ...)`:
  - For comment buffers

### 5.3 Keymaps
- [ ] Create `linear-mode-map` for minor mode:
  - Suggested prefix: `C-c l` (user can override)
  - Example bindings:
    - `C-c l c` -> `linear-create-issue`
    - `C-c l v` -> `linear-view-issue`
    - `C-c l l` -> `linear-list-project-issues`
    - `C-c l m` -> `linear-add-minicomment`
    - `C-c l C` -> `linear-add-comment`
    - `C-c l s` -> `linear-change-state`
    - `C-c l a` -> `linear-assign-issue`
    - `C-c l t` -> `linear-take-issue`
    - `C-c l e` -> `linear-export-issue`

- [ ] Create mode-specific keymaps for each major mode

### 5.4 Text Properties vs Overlays
**Recommendation: Use text properties for edit markers, overlays for UI enhancements**

- [ ] Edit markers: Use invisible text properties
  - Insert `<!-- DESCRIPTION START -->` markers as literal text
  - Add `(invisible . linear-marker)` text property
  - Set `(add-to-invisibility-spec 'linear-marker)` in buffer
  - This makes markers invisible but preserves them in buffer text

- [ ] UI enhancements (optional):
  - Use overlays to highlight editable regions with different face
  - Add `(overlay-put ov 'face 'linear-editable-region)` where applicable
  - Use `(make-overlay start end)` between START/END markers

### 5.5 Faces for Syntax Highlighting
- [ ] Define custom faces:
  - `(defface linear-editable-region '((t :background "#f0f0f0")) "Face for editable regions")`
  - `(defface linear-issue-id '((t :inherit font-lock-constant-face)) "Face for issue IDs")`
  - `(defface linear-metadata '((t :inherit font-lock-comment-face)) "Face for metadata")`

---

## 6. UI DIFFERENCES AND EMACS PARADIGMS

### 6.1 Selection Prompts: `completing-read` vs `vim.ui.select`

**Vim/Neovim:**
```lua
vim.ui.select(items, {
  prompt = 'Select team:',
  format_item = function(item) return item.name end
}, callback)
```

**Emacs equivalent:**
- [ ] Use `completing-read` with collection built from items:
  ```elisp
  (let* ((choices (mapcar (lambda (item)
                           (cons (format "%s (%s)" (alist-get 'name item) (alist-get 'key item))
                                 item))
                         items))
         (selection (completing-read "Select team: " choices nil t))
         (selected-item (cdr (assoc selection choices))))
    ...)
  ```
- [ ] Consider using `ivy-read`, `helm`, or `vertico` if user has them installed (check with `fboundp`)
- [ ] Create helper function `linear--select-from-list (prompt items format-fn)` that abstracts this

### 6.2 Input Prompts: `read-string` vs `input()`

**Vim/Neovim:**
```lua
vim.ui.input({prompt = 'Enter issue ID: '}, callback)
```

**Emacs equivalent:**
- [ ] Use `read-string`:
  ```elisp
  (let ((issue-id (read-string "Enter issue ID (e.g., ENG-123): ")))
    ...)
  ```
- [ ] No callback needed, synchronous is Emacs paradigm

### 6.3 Notifications: `message` vs `vim.notify`

**Vim/Neovim:**
```lua
vim.notify("Issue created!", vim.log.levels.INFO)
```

**Emacs equivalent:**
- [ ] Use `(message "Issue created!")` for INFO
- [ ] Use `(error "Failed to create issue")` for ERROR (signals an error, stops execution)
- [ ] Use `(warn "No projects found")` for WARN (logs to `*Messages*` buffer)
- [ ] Consider using `(display-warning 'linear "message" :warning)` for non-fatal warnings

### 6.4 Buffer Display: `switch-to-buffer` vs window management

**Vim/Neovim:**
```lua
vim.api.nvim_set_current_buf(bufnr)
```

**Emacs equivalent:**
- [ ] Use `(switch-to-buffer buffer)` to display in current window
- [ ] Use `(pop-to-buffer buffer)` to display intelligently (may split window)
- [ ] Use `display-buffer-alist` to customize where Linear buffers appear
- [ ] Consider using `(display-buffer buffer '((display-buffer-same-window)))` for more control

---

## 7. ASYNCHRONOUS CONSIDERATIONS (FUTURE)

### 7.1 Why Async Matters in Emacs
- Emacs is single-threaded; long-running CLI calls block UI
- Vim9 plugin uses synchronous calls (acceptable for Vim)
- Neovim uses async but Emacs needs it more due to UI architecture

### 7.2 Async Implementation with `make-process`
- [ ] Create `linear--run-cli-async (args callback)`:
  ```elisp
  (let ((process (make-process
                  :name "linear-cli"
                  :buffer "*linear-cli-output*"
                  :command (cons linear--cli-path (split-string args))
                  :sentinel (lambda (proc event)
                             (when (string= event "finished\n")
                               (with-current-buffer (process-buffer proc)
                                 (let ((output (buffer-string)))
                                   (funcall callback output nil))))))))
    process)
  ```
- [ ] Handle stderr separately with `:stderr` buffer
- [ ] Use process filter for streaming output if needed

### 7.3 Async UI Patterns
- [ ] Show "Fetching..." message immediately
- [ ] Use `(spinner-start)` if `spinner.el` is available (show spinner during async ops)
- [ ] Handle user interactions during async operations (avoid blocking)

---

## 8. ERROR HANDLING AND ROBUSTNESS

### 8.1 Error Handling Patterns
- [ ] Wrap CLI calls in `(condition-case err ... (error (message "Error: %s" err)))`
- [ ] Validate CLI path exists before running commands
- [ ] Handle JSON parse errors gracefully
- [ ] Handle empty/nil results from CLI
- [ ] Check buffer existence before operations

### 8.2 Input Validation
- [ ] Validate issue ID format with regex: `^[A-Z]+-[0-9]+$`
- [ ] Validate non-empty title before submission
- [ ] Validate team/project IDs exist before API calls

### 8.3 CLI Availability Checks
- [ ] Create `(defun linear--ensure-cli ()` that checks if CLI exists and is executable
- [ ] Call this before all CLI operations
- [ ] Display helpful error message if CLI not found (suggest installation)

---

## 9. TESTING AND DEVELOPMENT

### 9.1 Unit Tests with ERT
- [ ] Create `linear-test.el` file
- [ ] Write tests for:
  - JSON parsing: `(ert-deftest linear-test-parse-json () ...)`
  - Issue ID resolution: `(ert-deftest linear-test-resolve-issue-id () ...)`
  - Buffer parsing: `(ert-deftest linear-test-parse-issue-buffer () ...)`
  - CLI command building: `(ert-deftest linear-test-build-command () ...)`

### 9.2 Development Workflow
- [ ] Add `(eval-when-compile (require 'cl-lib))` to avoid compile warnings
- [ ] Use `(declare-function)` for functions not yet defined (forward declarations)
- [ ] Run `M-x checkdoc` to validate docstrings
- [ ] Run `M-x package-lint-current-buffer` to check package conventions
- [ ] Test on multiple Emacs versions (27.1+, 28.1+, 29.1+)

---

## 10. DOCUMENTATION AND PACKAGING

### 10.1 Docstrings
- [ ] Every public function (no `--` prefix) must have docstring
- [ ] Follow Emacs docstring conventions:
  - First line is complete sentence ending with period
  - Argument names in UPPER CASE
  - Example: `"Fetch issue with ISSUE-ID and display in buffer.\n\nIf ISSUE-ID is nil, resolve from context."`

### 10.2 Commentary Section
- [ ] Add extensive `;;; Commentary:` section explaining:
  - What the package does
  - How to install linear-cli
  - How to configure
  - Example usage
  - Keybindings

### 10.3 Package Distribution
- [ ] Create `linear-pkg.el` for MELPA
- [ ] Add proper `;;; linear.el ends here` footer
- [ ] Ensure byte-compilation is clean: `M-x byte-compile-file`
- [ ] Create README.md with installation instructions
- [ ] Add screenshots/demos (optional but helpful)

### 10.4 Customization
- [ ] Create `M-x customize-group RET linear RET` entry
- [ ] Add customization for:
  - `linear-export-dir`
  - `linear-cli-path` (allow override)
  - `linear-keymap-prefix` (default `C-c l`)
  - Faces for syntax highlighting

---

## 11. EMACS-SPECIFIC FEATURES (ENHANCEMENTS)

### 11.1 Org-mode Integration (Nice-to-have)
- [ ] Create `linear-org-link-type`:
  - Define custom link: `[[linear:ENG-123]]`
  - Open link opens issue in buffer: `(org-link-set-parameters "linear" :follow #'linear-view-issue)`
  - Export to markdown: `[ENG-123](https://linear.app/...)`

### 11.2 Completion at Point (Company/Completion)
- [ ] Define `linear-completion-at-point-function`:
  - Complete issue IDs from recent issues
  - Complete team names, project names in buffers
  - Add to `completion-at-point-functions`

### 11.3 Transient Menus (Alternative to Keymaps)
- [ ] Consider using `transient.el` (like Magit) for command interface:
  - `M-x linear` opens transient menu with all commands
  - Visual, discoverable, more user-friendly than keybindings

---

## 12. MIGRATION FROM VIM PATTERNS

### 12.1 Autocmd -> Hooks
**Vim:**
```vim
autocmd BufWriteCmd <buffer> call SubmitIssue()
```

**Emacs:**
```elisp
(add-hook 'write-contents-functions #'linear--submit-issue nil t)
```
- Fourth arg `t` makes it buffer-local

### 12.2 Buffer Variables
**Vim:**
```vim
setbufvar(bufnr, 'linear_issue_id', issue_id)
var issue_id = getbufvar(bufnr, 'linear_issue_id')
```

**Emacs:**
```elisp
(defvar-local linear-issue-id nil)
(setq linear-issue-id issue-id)  ; in buffer context
(buffer-local-value 'linear-issue-id buffer)  ; from elsewhere
```

### 12.3 String Operations
**Vim:** `trim()`, `join()`, `split()`
**Emacs:** `string-trim`, `mapconcat`, `split-string`

### 12.4 List/Array Operations
**Vim:** `add()`, `extend()`, `len()`
**Emacs:** `(push item list)`, `(append list1 list2)`, `(length list)`
**Note:** Emacs lists are backwards (cons cells), use `cl-lib` for better abstractions

---

## 13. FINAL CHECKLIST BEFORE RELEASE

- [ ] All public functions have docstrings
- [ ] All interactive commands have `(interactive)` spec
- [ ] Byte-compilation produces no warnings
- [ ] Package-lint produces no errors
- [ ] Checkdoc produces no errors
- [ ] All customization variables use `defcustom`
- [ ] All internal functions/variables use `--` naming convention
- [ ] README.md exists with installation and usage instructions
- [ ] All CLI commands are properly shell-escaped
- [ ] Error messages are clear and actionable
- [ ] Test suite passes (if implemented)
- [ ] Works on Emacs 27.1+ (current stable)
- [ ] Package header is complete and valid

---

## Key Implementation Notes

### JSON Handling
Use hash tables for JSON objects (faster lookups):
```elisp
(let ((json-object-type 'hash-table)
      (json-array-type 'list)
      (json-key-type 'string))
  (json-read-from-string output))
```

Access with: `(gethash "name" obj)` or `(map-elt obj "name")`

### Shell Escaping
Always use `shell-quote-argument` for user input:
```elisp
(format "create --title=%s" (shell-quote-argument title))
```

### Buffer-Local Hooks
Always use 4th argument `t` for buffer-local hooks:
```elisp
(add-hook 'write-contents-functions #'linear--submit-issue nil t)
```

### Line Number Mapping
Use 1-indexed line numbers (like Vim):
```elisp
(line-number-at-pos)  ; returns 1 for first line
```

### Message vs Error
- `(message "...")` - informational, doesn't stop execution
- `(error "...")` - signals error, stops execution, shows backtrace
- `(user-error "...")` - signals error but doesn't show backtrace (for expected user errors)
