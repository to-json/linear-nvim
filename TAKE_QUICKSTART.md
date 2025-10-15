# Quick Start: Take Command

## TL;DR

Self-assign issues without looking up your user ID.

## CLI Usage

```bash
# Take an issue
linear-cli take SRE-123

# Output:
# Issue taken successfully
#   Issue: SRE-123
#   Title: Your Issue Title
#   Assigned to: Your Name (your.email@example.com)
```

## Vim Usage

```vim
" Explicit ID
:LinearTake SRE-123

" Context-aware (from current buffer or line)
:LinearTake

" While viewing an issue - just run:
:LinearTake

" While in issue list - move cursor to issue line and run:
:LinearTake
```

## How It Works

**CLI**: Fetches your user ID automatically, then assigns the issue to you.

**Vim**: Smart enough to figure out which issue you mean:
1. Uses the ID you provide
2. Uses the issue you're viewing
3. Uses the issue your cursor is on (in issue lists)
4. Asks you if it can't figure it out

## Installation

Already installed! The `take` command is now part of your linear-cli.

```bash
# Rebuild if needed
cd /Users/jaesaxon/hc/linear-cli
go build
```

## Examples

### CLI Example
```bash
$ linear-cli take SRE-456
Issue taken successfully
  Issue: SRE-456
  Title: Fix broken API endpoint
  Assigned to: John Doe (john@example.com)
```

### Vim Example
```vim
" Open project issues
:LinearProjectIssues

" Navigate to an issue line (e.g., line with "SRE-456")
" Take it with one command:
:LinearTake

" Or view an issue first:
:LinearViewIssue SRE-456

" Then take it without repeating the ID:
:LinearTake
```

## Comparison to Assign

| Command | Usage | User ID |
|---------|-------|---------|
| `assign` | `linear-cli assign SRE-123 <user-id>` | Must provide |
| `take` | `linear-cli take SRE-123` | Auto-detected |

Same in Vim:
- `:LinearAssign SRE-123` - prompts for team member selection
- `:LinearTake SRE-123` - assigns to you immediately

## Auto-Refresh

When you take an issue you're currently viewing in Vim, the buffer automatically refreshes to show the new assignment.

## That's It

Just use `take` instead of manually assigning to yourself. Done.
