# linear-cli

Command-line interface for Linear's project management API. Built in Go with zero dependencies, provides fast access to issues, projects, and templates.

This project was basically fully vibe-coded. I've read some of the code, i guess. but mostly i just talked claude into it.

## Building

### Prerequisites

- Go 1.25.1 or later
- LINEAR_API_KEY environment variable

### Installation

Install using `go install`:

```bash
export LINEAR_API_KEY="lin_api_..."
go install github.com/yourusername/linear-cli@latest
```

Or build locally:

```bash
git clone <repo-url>
cd linear-cli
go build -o linear-cli .
```

The binary will be in your `$GOPATH/bin` (if using `go install`) or in the current directory.

### API Key Setup

Get your API key from https://linear.app/settings/api

```bash
export LINEAR_API_KEY="lin_api_..."
# Add to ~/.bashrc or ~/.zshrc for persistence
echo 'export LINEAR_API_KEY="lin_api_..."' >> ~/.bashrc
```

## Commands

### List Issues (default)

```bash
linear-cli                          # List your assigned non-completed issues
linear-cli --json                   # JSON output
```

### Projects

```bash
linear-cli projects                 # List your active projects
linear-cli projects --all           # List all workspace projects
linear-cli projects --json          # JSON output
```

### Templates

```bash
linear-cli templates                # List templates for your teams
linear-cli get template <id>        # Show template details
```

### Teams

```bash
linear-cli teams                    # List your teams
linear-cli states <team-id>         # Show workflow states for team
linear-cli team-members <team-id>   # List team members
```

### Create Issues

```bash
linear-cli create                   # Interactive issue creation
```

Walks you through team selection, template selection (optional), project assignment (optional), and issue details.

### View & Update Issues

```bash
linear-cli get issue <id>           # Show full issue details
linear-cli update-state <issue-id> <state-id>    # Change issue state
linear-cli assign <issue-id> <user-id>           # Assign to user
linear-cli unassign <issue-id>                   # Remove assignee
linear-cli update-description <issue-id> "text"  # Update description
```

### Comments

```bash
linear-cli comment <issue-id> "comment text"     # Add comment
linear-cli add-comment <issue-id> "text"         # Alias for comment
linear-cli update-comment <comment-id> "text"    # Edit existing comment
```

### Project Issues

```bash
linear-cli issues <project-id>      # List all issues in project
linear-cli project-issues <project-id>  # Alias
```

All commands support `--json` flag for machine-readable output.

## Vim Plugin

The included `linear.lua` plugin provides integrated Linear workflows inside Neovim.

### Installation

#### Using lazy.nvim

```lua
{
  dir = "~/hc/linear-cli",  -- or wherever you cloned it
  ft = "markdown",
  config = function()
    require("linear").setup()
  end,
}
```

#### Using packer.nvim

```lua
use {
  "~/hc/linear-cli",
  config = function()
    require("linear").setup()
  end
}
```

### Plugin Commands

- `:LinearCreate` - Interactive issue creation with template support
- `:LinearProjectIssues` - Browse issues by project, press `<CR>` to view
- `:LinearViewIssue [id]` - View issue with full details (editable)
- `:LinearAddComment [id]` - Add comment to issue
- `:LinearChangeState [id]` - Change issue workflow state
- `:LinearAssign [id]` - Assign issue to team member
- `:LinearUnassign [id]` - Remove assignee

### Editing Issues

When viewing an issue with `:LinearViewIssue`, you can:

1. Edit the description (marked with `<!-- DESCRIPTION START/END -->`)
2. Edit your own comments (marked with `<!-- COMMENT ID: ... -->`)
3. Save changes with `:w` - automatically syncs to Linear
4. Changes are auto-refreshed after saving

### Plugin Requirements

- Neovim 0.7+
- `linear-cli` binary in `$GOPATH/bin` or local path
- LINEAR_API_KEY environment variable

The plugin automatically finds the CLI in your GOPATH or falls back to the local build.

## Making Changes

### Architecture

The codebase follows a clean separation pattern:

- **Query Builders** (`build*Query()`) - Generate GraphQL queries
- **GraphQL Client** (`client.go`) - HTTP execution and error handling
- **Response Parsers** (`parse*Response()`) - Parse JSON responses
- **Public API Functions** (`get*()`, `print*()`) - High-level operations
- **Main Entry** (`main.go`) - Command routing

### Adding a New Command

Example: Adding a `labels` command to list labels

1. **Add Query Builder** (in `main.go`):

```go
func buildLabelsQuery() string {
    return `
query GetLabels {
  workspaceLabels(first: 100) {
    nodes {
      id
      name
      color
    }
  }
}
`
}
```

2. **Add Parser**:

```go
func parseLabelsResponse(data json.RawMessage) ([]Label, error) {
    var resp struct {
        WorkspaceLabels struct {
            Nodes []Label `json:"nodes"`
        } `json:"workspaceLabels"`
    }
    if err := json.Unmarshal(data, &resp); err != nil {
        return nil, fmt.Errorf("failed to unmarshal: %w", err)
    }
    return resp.WorkspaceLabels.Nodes, nil
}
```

3. **Add Public Function**:

```go
func getLabels(apiKey string) ([]Label, error) {
    data, err := executeGraphQL(apiKey, buildLabelsQuery(), nil)
    if err != nil {
        return nil, err
    }
    return parseLabelsResponse(data)
}
```

4. **Add Command Handler** (in `main()`):

```go
if len(os.Args) > 1 && os.Args[1] == "labels" {
    labels, err := getLabels(apiKey)
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error: %v\n", err)
        os.Exit(1)
    }
    printLabels(labels)
    return
}
```

5. **Add Data Model** (if needed):

```go
type Label struct {
    ID    string `json:"id"`
    Name  string `json:"name"`
    Color string `json:"color"`
}
```

### Testing

```bash
# Run all tests
go test -v

# Run specific test
go test -v -run TestGetIssue

# With coverage
go test -cover
```

Tests use mock HTTP servers to isolate GraphQL operations. See `tests/` directory.

### GraphQL Reference

- Schema: `claude-docs/schema.graphql`
- API Guide: `claude-docs/linear-graphql-guide.md`
- Endpoint: https://api.linear.app/graphql

### Code Style

- Use `build*Query()` for query/mutation builders
- Use `parse*Response()` for response parsing
- Use `get*()` for public API functions
- Use `print*()` for human-readable output
- Support `--json` flag for all commands
- Keep functions focused and testable

### Project Structure

```
linear-cli/
├── main.go                 # Core CLI logic & command handlers
├── client.go               # GraphQL HTTP client
├── comment_additions.go    # Comment-related operations
├── linear.lua              # Neovim plugin
├── go.mod                  # Go module definition
├── claude-docs/            # API reference & docs
└── tests/                  # Test files
```

## Documentation

- Full project context: `claude-docs/claude_project_context.md`
- GraphQL API guide: `claude-docs/linear-graphql-guide.md`
- Linear API docs: https://developers.linear.app/docs/graphql/working-with-the-graphql-api

## Features

**Implemented:**
- Issue listing (assigned, by project)
- Project management
- Template browsing
- Interactive issue creation
- Issue state updates
- Issue assignment
- Comment management
- Team & member queries
- JSON output for scripting
- Neovim integration with editing support

**Not Yet Implemented:**
- Pagination (limited to first 100 results)
- Issue search/filtering
- Label management
- Due date handling
- Cycle management
- Bulk operations
- Markdown export

