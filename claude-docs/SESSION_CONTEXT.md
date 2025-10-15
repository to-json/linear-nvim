# Linear CLI Project - Context Summary

**Date:** 2025-10-15
**Location:** `/Users/jaesaxon/hc/linear-cli/`

---

## Project Overview

Linear CLI is a command-line tool and editor integration for Linear's project management API. Built in Go with zero dependencies, it provides fast access to issues, projects, and templates. The project includes three complete implementations:

1. **CLI tool** (`main.go`) - Go-based CLI
2. **Neovim plugin** (`linear.lua`) - Async Lua plugin
3. **Vim9 plugin** (`linear.vim`) - Blocking vim9script plugin

---

## Current State

### ✅ Fully Working Components

#### CLI (`linear-cli` binary)
- **Location:** `/Users/jaesaxon/hc/linear-cli/linear-cli`
- **Built:** 2025-10-15 (recent build with all latest changes)
- **Features:**
  - Issue listing, viewing, creation, updating
  - Non-interactive create with flags (`--team`, `--title`, `--description`, `--priority`, `--project`)
  - Interactive create (prompts for team, template, project)
  - Comment management
  - State changes, assignment, self-assignment (take)
  - Team/project queries
  - Parent/child issue relationships
  - JSON output for all commands

**Key Addition:** Non-interactive `create` command now accepts flags (added this session). If flags provided, skips interactive mode.

#### Neovim Plugin (`linear.lua`)
- **Location:** `/Users/jaesaxon/hc/linear-cli/linear.lua`
- **Status:** Fully functional, production-ready
- **Features:**
  - Async operations (non-blocking)
  - Context-aware commands (resolve issue ID from buffer, cursor, or prompt)
  - Editable issue buffers (edit description, edit own comments)
  - Issue creation with templates
  - Issue export to markdown
  - Project issue listing
  - All CLI operations wrapped
  - Sub-issue rendering (parent/child relationships)
  - Auto-refresh after changes

**Installation:**
```lua
-- lazy.nvim
{
  dir = "~/hc/linear-cli",
  config = function()
    require("linear").setup({
      export_dir = "~/.linear/past-tickets"
    })
  end,
}
```

#### Vim9 Plugin (`linear.vim`)
- **Location:** `/Users/jaesaxon/hc/linear-cli/linear.vim`
- **Status:** Fully functional, tested
- **Features:** Same as Neovim plugin but synchronous (blocking)
- **Fixed Issues (this session):**
  - Funcref parameter name must be capitalized (`Format_func`)
  - `<SID>` expansion in autocmds (now uses captured `SID` variable)
  - Command registration (uses `<SID>` instead of `linear#` prefix)

**Installation:**
```vim
" .vimrc
source ~/hc/linear-cli/linear.vim
" Commands auto-register
```

---

## Recent Work (This Session)

### 1. Context-Aware Refactoring
- Created `resolve_issue_id()` helper in `linear.lua` (lines 67-107)
- Refactored 6 functions to use it: `view_issue`, `add_comment`, `take_issue`, `change_state`, `assign_issue`, `unassign_issue`
- Added `skip_buffer` parameter for `view_issue` to avoid re-opening same issue

### 2. Comment Functionality Split
- Renamed original `add_comment()` to `add_minicomment()` (modal prompt)
- Created new buffer-based `add_comment()` with full editor
- Both commands registered: `:LinearComment` (buffer), `:LinearMiniComment` (modal)

### 3. Issue Export Feature
- Added `export_issue()` function (Lua and Vim9)
- Exports to `~/.linear/past-tickets/ISSUE-ID.md` (configurable)
- Clean markdown without edit markers
- Includes all comments (not just editable ones)
- Directory creation with `mkdir -p` behavior
- File existence check with overwrite prompt

### 4. Parent/Child Issue Relationships
- Updated GraphQL query in `buildGetIssueQuery()` (main.go lines 187-201)
- Added Parent and Children fields to Issue struct (main.go lines 800-814)
- Rendering in CLI `printIssueDetails()` (main.go lines 1931-1952)
- Rendering in Neovim plugin `view_issue()` (linear.lua lines 975-992)
- Rendering in Vim9 plugin `ViewIssue()` (linear.vim)
- Rendering in export functions

**Bug Fixed:** Sub-issues weren't showing in Neovim - issue was stale CLI binary. Rebuilt with `go build -o linear-cli .`

### 5. Non-Interactive Create Command
- Added `nonInteractiveCreateIssue()` function to main.go (lines 1529-1591)
- Modified main() to detect flags and route to non-interactive mode (lines 993-1016)
- Updated Lua plugin to use new CLI interface
- Now supports programmatic issue creation from plugins

### 6. Vim9 Port
- Created complete vim9script port (`linear.vim`)
- 1327 lines, feature-complete
- Fixed funcref naming, `<SID>` expansion issues
- All commands working

### 7. Documentation Updates
- Updated README.md to document both plugins (Neovim + Vim9)
- Added Configuration section
- Updated Features list (added parent/child, export)
- Updated Project Structure
- Removed "Markdown export" from Not Yet Implemented

### 8. Emacs Port Planning
- Created comprehensive todo list: `EMACS_PORT_TODO.md`
- Covers all aspects: core infrastructure, buffer management, commands, Emacs-specific paradigms
- Includes code examples and migration notes from Vim patterns

---

## File Structure

```
linear-cli/
├── main.go                          # CLI core + command handlers
├── client.go                        # GraphQL HTTP client
├── comment_additions.go             # Comment operations
├── linear-cli                       # Compiled binary (REBUILD after Go changes!)
├── linear.lua                       # Neovim plugin (async, production-ready)
├── linear.vim                       # Vim9 plugin (blocking, production-ready)
├── linear-original.vim              # First vim9script attempt (superseded by linear.vim)
├── go.mod                           # Go dependencies
├── README.md                        # Main documentation (updated)
├── SESSION_CONTEXT.md               # This file
├── EMACS_PORT_TODO.md               # Emacs port checklist
├── PORT_TO_VIM9_COMMAND.txt         # Subagent command for vim9 validation
├── claude-docs/                     # API reference
│   ├── schema.graphql               # Linear GraphQL schema
│   ├── linear-graphql-guide.md      # API guide
│   └── claude_project_context.md    # Project context
└── tests/                           # Test files
```

---

## Key Design Decisions

### Context-Aware Issue Resolution
**Priority order:**
1. Explicit argument (e.g., `:LinearViewIssue ENG-123`)
2. Buffer variable `linear_issue_id` (when viewing an issue)
3. Issue map + cursor position (when in issue list)
4. Prompt user for input (fallback)

**Special case:** `ViewIssue` uses `skip_buffer = true` to avoid re-opening the same issue when already viewing one.

### Buffer Naming Conventions
- Issue view: `linear://ISSUE-ID`
- Issue list: `linear-project-issues://PROJECT-NAME`
- Comment: `linear-comment://ISSUE-ID`
- Create: `linear-create://TEAM-KEY/PROJECT-NAME`

### Edit Markers
- Description: `<!-- DESCRIPTION START -->` ... `<!-- DESCRIPTION END -->`
- Comments: `<!-- COMMENT ID: xxx START -->` ... `<!-- COMMENT ID: xxx END -->`
- Only user's own comments are editable (matched by author name)

### CLI Binary Location
**Discovery order:**
1. `$GOPATH/bin/linear-cli` (checked first)
2. `~/hc/linear-cli/linear-cli` (fallback)

Both plugins use this logic via `find_cli()` / `FindCli()`.

---

## Important Gotchas

### 1. Rebuild CLI After Go Changes
**Critical:** If you modify `main.go`, `client.go`, or any Go files, you MUST rebuild:
```bash
cd /Users/jaesaxon/hc/linear-cli
go build -o linear-cli .
```

Otherwise plugins will call stale binary with old behavior. This bit us with sub-issues not showing.

### 2. Vim9 Function Naming
- Funcref parameters must start with capital letter: `Format_func`, not `format_func`
- `<SID>` doesn't expand in strings - must capture as variable first: `const SID = expand('<SID>')`

### 3. Linear API Requires API Key
Set environment variable:
```bash
export LINEAR_API_KEY="lin_api_..."
# Add to ~/.bashrc or ~/.zshrc for persistence
```

Get key from: https://linear.app/settings/api

### 4. Context Awareness Requires Buffer Variables
The plugins set buffer-local variables to enable context awareness:
- `linear_issue_id` - Set when viewing an issue
- `linear_issue_map` - Set in issue lists (maps line number to issue ID)
- `linear_issue_data` - Original issue data for comparison on save

### 5. Go Install Path
README recommends `go install` but currently using local build. To install globally:
```bash
go install
# Binary goes to $GOPATH/bin/linear-cli
```

---

## Common Commands

### CLI Usage
```bash
# List your issues
./linear-cli

# View issue
./linear-cli get issue ENG-123

# Create issue (interactive)
./linear-cli create

# Create issue (non-interactive)
./linear-cli create --team=TEAM_ID --title="Issue title" --description="Details" --priority=2

# Take issue (self-assign)
./linear-cli take ENG-123

# Add comment
./linear-cli comment ENG-123 "Comment text"

# List projects
./linear-cli projects

# All commands support --json flag
./linear-cli get issue ENG-123 --json
```

### Neovim Plugin Commands
```vim
:LinearCreate              " Interactive issue creation
:LinearViewIssue [id]      " View issue (context-aware)
:LinearComment [id]        " Add comment (buffer editor)
:LinearMiniComment [id]    " Add comment (quick modal)
:LinearTake [id]           " Self-assign issue
:LinearChangeState [id]    " Change workflow state
:LinearAssign [id]         " Assign to team member
:LinearUnassign [id]       " Remove assignee
:LinearExport [id]         " Export to markdown
:LinearProjectIssues       " Browse project issues
```

### Vim9 Plugin Commands
Same as Neovim, but operations block until complete.

---

## Configuration

### Neovim (linear.lua)
```lua
require("linear").setup({
  export_dir = "~/.linear/past-tickets"  -- default shown
})
```

### Vim9 (linear.vim)
Commands auto-register on source. No explicit setup needed unless customizing export directory.

---

## Testing

### Test BADID-1 Issue
There's a test issue `BADID-1` with a sub-issue `BADID-2`. Used throughout development for testing parent/child relationships.

### Test Commands
```bash
# Verify CLI works
./linear-cli get issue BADID-1 --json | jq '.children'

# Test sub-issue rendering in Neovim
:LinearViewIssue BADID-1
# Should show "Sub-Issues" section with BADID-2

# Test create with flags
./linear-cli create --team='...' --title='test' --priority=0
```

---

## Known Issues / TODO

### None Critical
All major functionality working. Potential future enhancements:

1. **Pagination** - Currently limited to first 100 results
2. **Search/filtering** - No search functionality yet
3. **Label management** - Labels not implemented
4. **Due dates** - Not handling due dates
5. **Cycles** - Cycle management not implemented
6. **Bulk operations** - No bulk issue updates

### Nice-to-Have
- Async operations for Vim9 version (currently blocking)
- More keyboard shortcuts in issue view buffers
- Syntax highlighting for issue IDs in buffers
- Integration with other tools (fzf, telescope, etc.)

---

## Development Workflow

### Making Changes to CLI
1. Edit Go files (`main.go`, etc.)
2. **Rebuild:** `go build -o linear-cli .`
3. Test with CLI: `./linear-cli <command>`
4. Test with plugin: `:LinearViewIssue BADID-1`

### Making Changes to Neovim Plugin
1. Edit `linear.lua`
2. Reload in Neovim: `:luafile ~/.config/nvim/lua/linear.lua` or restart
3. Test commands: `:LinearViewIssue BADID-1`

### Making Changes to Vim9 Plugin
1. Edit `linear.vim`
2. Reload in Vim: `:source ~/hc/linear-cli/linear.vim`
3. Test commands: `:LinearViewIssue BADID-1`

### Using Subagents for Complex Tasks
This session used subagents for:
- Implementing take command (context-aware)
- Refactoring context resolution
- Implementing sub-issue rendering
- Creating Emacs port todo list

Pattern: Use `Task` tool with appropriate subagent type when task is complex or would benefit from focused implementation.

---

## Next Steps (If Resuming Work)

### Immediate Priorities
None - all major features working.

### If Implementing New Features
1. Check if CLI needs changes (add to `main.go`)
2. Add corresponding functions to plugins (`linear.lua`, `linear.vim`)
3. Update README.md
4. Test with `BADID-1` issue
5. Rebuild CLI if Go changes made

### If Porting to Emacs
- Follow `EMACS_PORT_TODO.md`
- Start with core infrastructure (sections 1-2)
- Use synchronous `call-process` initially
- Reference `linear.vim` for blocking implementation patterns

---

## Contact / References

- Linear API Docs: https://developers.linear.app/docs/graphql/working-with-the-graphql-api
- GraphQL Schema: `claude-docs/schema.graphql`
- API Guide: `claude-docs/linear-graphql-guide.md`
- Project Context: `claude-docs/claude_project_context.md`

---

## Quick Reference: Most Used Files

**To modify CLI behavior:**
- `main.go` - Command handlers and main logic

**To modify Neovim plugin:**
- `linear.lua` - All plugin functionality

**To modify Vim9 plugin:**
- `linear.vim` - All plugin functionality

**To understand API:**
- `claude-docs/schema.graphql` - Complete GraphQL schema
- `claude-docs/linear-graphql-guide.md` - API usage examples

**After ANY changes:**
```bash
# If Go changed
go build -o linear-cli .

# If Lua changed
:luafile path/to/linear.lua  # in Neovim

# If Vim9 changed
:source path/to/linear.vim  # in Vim
```

---

## Session Summary

**What was accomplished:**
- Fixed vim9script issues (funcref naming, SID expansion)
- Added non-interactive create command to CLI
- Implemented parent/child issue relationships across all components
- Created issue export functionality
- Split comment functionality (buffer vs modal)
- Refactored context-aware issue resolution
- Updated all documentation
- Created Emacs port planning document

**All components are production-ready and fully functional.**
