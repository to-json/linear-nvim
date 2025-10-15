# Implementation Summary: `take` Command

## What Was Implemented

A `take` command that self-assigns Linear issues to the current user in both the CLI and Vim plugin.

## Changes Made

### 1. main.go - CLI Implementation

**Location**: Lines 1059-1087 (command handler), Lines 1786-1799 (function)

**Added command handler**:
```go
if len(os.Args) > 1 && os.Args[1] == "take" {
    if len(os.Args) < 3 {
        fmt.Fprintln(os.Stderr, "Usage: linear-cli take <issue-id> [--json]")
        os.Exit(1)
    }
    issueID := os.Args[2]
    jsonOutput := hasJSONFlag()

    issue, err := takeIssue(apiKey, issueID)
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error taking issue: %v\n", err)
        os.Exit(1)
    }

    if jsonOutput {
        if err := outputJSON(issue); err != nil {
            fmt.Fprintf(os.Stderr, "Error encoding JSON: %v\n", err)
            os.Exit(1)
        }
    } else {
        fmt.Printf("Issue taken successfully\n")
        fmt.Printf("  Issue: %s\n", issue.Identifier)
        fmt.Printf("  Title: %s\n", issue.Title)
        if issue.Assignee != nil {
            fmt.Printf("  Assigned to: %s (%s)\n", issue.Assignee.Name, issue.Assignee.Email)
        }
    }
    return
}
```

**Added takeIssue function**:
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

### 2. linear.lua - Vim Plugin Implementation

**Location**: Lines 1111-1173 (function), Lines 1209-1214 (command registration)

**Added M.take_issue function** with context-aware logic:
- Checks for explicit argument
- Checks `linear_issue_id` buffer variable (issue view)
- Checks `linear_issue_map` buffer variable (issue list)
- Falls back to user input prompt
- Auto-refreshes buffer after successful assignment

**Added command registration**:
```lua
vim.api.nvim_create_user_command('LinearTake', function(opts)
  M.take_issue(opts.args)
end, {
  nargs = '?',
  desc = 'Take (self-assign) a Linear issue to yourself',
})
```

## Usage

### CLI
```bash
linear-cli take <issue-id>         # Take an issue
linear-cli take <issue-id> --json  # Take with JSON output
```

### Vim Plugin
```vim
:LinearTake SRE-123    # Explicit issue ID
:LinearTake            # Context-aware (from buffer or prompt)
```

## How It Works

1. **CLI**: Calls viewer query to get current user ID, then assigns issue to that ID
2. **Vim**: Context-aware argument resolution, calls CLI command, auto-refreshes buffer

## Context-Aware Behavior (Vim)

Priority order for determining issue ID:
1. Explicit argument: `:LinearTake SRE-123`
2. Buffer variable `linear_issue_id` (when viewing an issue)
3. Buffer variable `linear_issue_map[cursor_line]` (when in issue list)
4. User input prompt

## Pattern Consistency

Follows existing patterns:
- **CLI**: Similar to `assign` command (lines 1000-1028)
- **Vim context-aware args**: Similar to `add_comment` function (lines 476-530)
- **Auto-refresh**: Similar to `assign_issue` function (lines 986-1067)

## Testing

Build and test:
```bash
cd /Users/jaesaxon/hc/linear-cli
go build
./linear-cli take           # Shows usage
./linear-cli take SRE-123   # Takes the issue (requires valid LINEAR_API_KEY)
```

Vim testing:
```vim
:source linear.lua
:call v:lua.require('linear').setup()
:LinearTake SRE-123
```

## Files Modified

- **main.go**: Added CLI command handler and `takeIssue()` function
- **linear.lua**: Added `M.take_issue()` function and `:LinearTake` command

## Files Created (Documentation)

- **TAKE_COMMAND.md**: Detailed documentation of the feature
- **IMPLEMENTATION_SUMMARY.md**: This file
- **test_take.sh**: Simple test script to verify implementation
