# Take Command Implementation

This document describes the implementation of the `take` command for the Linear CLI and Vim plugin.

## Overview

The `take` command allows you to quickly self-assign Linear issues without needing to look up your user ID. It automatically fetches your viewer ID and assigns the issue to you.

## CLI Usage

```bash
linear-cli take <issue-id> [--json]
```

### Examples

```bash
# Take an issue
linear-cli take SRE-123

# Take an issue with JSON output
linear-cli take SRE-123 --json
```

### Output

```
Issue taken successfully
  Issue: SRE-123
  Title: Fix production bug
  Assigned to: Your Name (your.email@example.com)
```

## Vim Plugin Usage

```vim
:LinearTake [issue-id]
```

### Context-Aware Behavior

The Vim plugin is context-aware and will try to determine the issue ID in the following order:

1. **Explicit argument**: If you provide an issue ID, it uses that
   ```vim
   :LinearTake SRE-123
   ```

2. **Issue view buffer**: If you're viewing an issue (buffer has `linear_issue_id` variable), it uses that
   ```vim
   " While viewing an issue
   :LinearTake
   ```

3. **Issue list buffer**: If you're in an issue list (buffer has `linear_issue_map` variable), it uses the issue on the current line
   ```vim
   " While browsing project issues
   " Place cursor on an issue line and run:
   :LinearTake
   ```

4. **User prompt**: If none of the above work, it prompts you for the issue ID
   ```vim
   :LinearTake
   " Prompts: Enter issue ID (e.g., ENG-123):
   ```

### Auto-Refresh

After successfully taking an issue, if you're currently viewing that issue in a buffer, the buffer will automatically refresh to show the updated assignment.

## Implementation Details

### CLI Implementation (main.go)

The CLI command handler:

```go
if len(os.Args) > 1 && os.Args[1] == "take" {
    if len(os.Args) < 3 {
        fmt.Fprintln(os.Stderr, "Usage: linear-cli take <issue-id> [--json]")
        os.Exit(1)
    }
    issueID := os.Args[2]
    jsonOutput := hasJSONFlag()

    issue, err := takeIssue(apiKey, issueID)
    // ... error handling and output
}
```

The `takeIssue` function:

```go
func takeIssue(apiKey string, issueID string) (*Issue, error) {
    // First, get the viewer's ID
    viewerData, err := executeGraphQL(apiKey, buildViewerIDQuery(), nil)
    if err != nil {
        return nil, fmt.Errorf("failed to get viewer ID: %w", err)
    }
    viewerID, err := parseViewerIDResponse(viewerData)
    if err != nil {
        return nil, fmt.Errorf("failed to parse viewer ID: %w", err)
    }

    // Now assign the issue to the viewer
    return assignIssue(apiKey, issueID, viewerID)
}
```

### Vim Plugin Implementation (linear.lua)

The `M.take_issue` function implements the context-aware logic:

```lua
function M.take_issue(issue_id)
  if not issue_id or issue_id == "" then
    -- Try to get from current buffer's linear_issue_id
    local ok, buf_issue_id = pcall(vim.api.nvim_buf_get_var, 0, 'linear_issue_id')
    if ok and buf_issue_id and buf_issue_id ~= "" then
      issue_id = buf_issue_id
    else
      -- Try to get from issue list buffer
      local map_ok, issue_map = pcall(vim.api.nvim_buf_get_var, 0, 'linear_issue_map')
      if map_ok and issue_map then
        local cursor = vim.api.nvim_win_get_cursor(0)
        local line_num = cursor[1]
        if issue_map[line_num] then
          issue_id = issue_map[line_num]
        end
      end
    end

    -- If still no issue_id, prompt for it
    if not issue_id or issue_id == "" then
      vim.ui.input({
        prompt = 'Enter issue ID (e.g., ENG-123): ',
      }, function(input)
        if input and input ~= "" then
          M.take_issue(input)
        end
      end)
      return
    end
  end

  vim.notify("Taking issue " .. issue_id .. "...", vim.log.levels.INFO)

  local cmd = string.format(
    'take %s --json',
    vim.fn.shellescape(issue_id)
  )

  run_cli(cmd, function(taken_issue, err)
    if err then
      vim.notify("Error taking issue: " .. err, vim.log.levels.ERROR)
      return
    end

    local assignee_name = "Unknown"
    if taken_issue.assignee and type(taken_issue.assignee) == "table" then
      assignee_name = taken_issue.assignee.name or "Unknown"
    end

    vim.notify("Issue taken successfully, assigned to: " .. assignee_name, vim.log.levels.INFO)

    -- Auto-refresh if we're viewing this issue
    local current_buf = vim.api.nvim_get_current_buf()
    local ok, buf_issue_id = pcall(vim.api.nvim_buf_get_var, current_buf, 'linear_issue_id')
    if ok and buf_issue_id == issue_id then
      vim.schedule(function()
        M.view_issue(issue_id)
      end)
    end
  end)
end
```

Command registration:

```lua
vim.api.nvim_create_user_command('LinearTake', function(opts)
  M.take_issue(opts.args)
end, {
  nargs = '?',
  desc = 'Take (self-assign) a Linear issue to yourself',
})
```

## GraphQL Operations

The `take` command uses two existing GraphQL operations:

1. **Viewer Query** - Gets the current user's ID:
   ```graphql
   query { viewer { id } }
   ```

2. **Assign Issue Mutation** - Assigns the issue (via existing `assignIssue` function):
   ```graphql
   mutation AssignIssue($id: String!, $assigneeId: String!) {
     issueUpdate(
       id: $id
       input: { assigneeId: $assigneeId }
     ) {
       success
       issue {
         id
         identifier
         title
         assignee { id name email }
       }
     }
   }
   ```

## Testing

Build the CLI:
```bash
go build
```

Test CLI command:
```bash
./linear-cli take SRE-123
```

Test in Vim:
```vim
" Load the plugin
:source linear.lua
:call v:lua.require('linear').setup()

" Test the command
:LinearTake SRE-123
```

## Comparison with Existing Commands

The `take` command follows the same patterns as:

- **CLI**: Similar to `assign` command but fetches viewer ID automatically
- **Vim Plugin**: Similar to `add_comment` for context-aware argument handling
- **Refresh**: Similar to `assign_issue` for auto-refresh pattern

## Files Modified

- `/Users/jaesaxon/hc/linear-cli/main.go` - Added CLI command handler and `takeIssue` function
- `/Users/jaesaxon/hc/linear-cli/linear.lua` - Added `M.take_issue` function and `:LinearTake` command registration
