package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"
)

// Helper function to check if --json flag is present
func hasJSONFlag() bool {
	for _, arg := range os.Args {
		if arg == "--json" {
			return true
		}
	}
	return false
}

// Helper function to output JSON
func outputJSON(v interface{}) error {
	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	return enc.Encode(v)
}

// GraphQL Query Builders

// buildViewerIDQuery returns a query to get the current user's ID
func buildViewerIDQuery() string {
	return `query { viewer { id } }`
}

// buildMyIssuesQuery returns a query to get the current user's assigned issues
func buildMyIssuesQuery() string {
	return `
query ViewerIssues {
  viewer {
    id
    name
    email
    assignedIssues(
      first: 100
      filter: {
        state: { type: { neq: "completed" } }
      }
    ) {
      nodes {
        id
        identifier
        title
        description
        priority
        state {
          name
          type
        }
        assignee {
          name
        }
        team {
          name
          key
        }
        createdAt
        updatedAt
        url
      }
    }
  }
}
`
}

// buildProjectsQuery returns a query to get projects with optional membership filtering
func buildProjectsQuery(viewerID string, includeMemberFilter bool) string {
	filterClause := `status: { type: { neq: "completed" } }`
	if includeMemberFilter && viewerID != "" {
		filterClause = `
      and: [
        { status: { type: { neq: "completed" } } }
        { members: { some: { id: { eq: "` + viewerID + `" } } } }
      ]`
	}

	return `
query GetProjects {
  projects(
    first: 100
    filter: {
      ` + filterClause + `
    }
  ) {
    nodes {
      id
      name
      description
      progress
      priority
      status {
        type
        name
      }
      lead {
        name
      }
      createdAt
      updatedAt
      url
    }
  }
}
`
}

// buildMyTeamsQuery returns a query to get the current user's teams
func buildMyTeamsQuery() string {
	return `
query GetMyTeams {
  viewer {
    teams(first: 100) {
      nodes {
        id
        name
        key
      }
    }
  }
}
`
}

// buildTemplatesQuery returns a query to get all templates
func buildTemplatesQuery() string {
	return `
query GetTemplates {
  templates {
    id
    name
    description
    type
    createdAt
    team {
      id
      name
      key
    }
  }
}
`
}

// buildGetIssueQuery returns a query to get a specific issue by ID
func buildGetIssueQuery() string {
	return `
query GetIssue($id: String!) {
  issue(id: $id) {
    id
    identifier
    title
    description
    priority
    estimate
    url
    createdAt
    updatedAt
    completedAt
    state {
      name
      type
    }
    assignee {
      name
      email
    }
    creator {
      name
    }
    team {
      id
      name
      key
    }
    labels {
      nodes {
        name
        color
      }
    }
    comments {
      nodes {
        id
        body
        createdAt
        user {
          name
        }
      }
    }
  }
}
`
}

// buildGetTemplateQuery returns a query to get a specific template by ID
func buildGetTemplateQuery() string {
	return `
query GetTemplate($id: String!) {
  template(id: $id) {
    id
    name
    description
    type
    templateData
    createdAt
    updatedAt
    team {
      id
      name
      key
    }
    creator {
      name
    }
  }
}
`
}

// buildTeamTemplatesQuery returns a query to get templates for a specific team
func buildTeamTemplatesQuery() string {
	return `
query GetTeamTemplates($teamId: String!) {
  team(id: $teamId) {
    templates(first: 100) {
      nodes {
        id
        name
        description
        type
        templateData
      }
    }
  }
}
`
}

// buildTeamProjectsQuery returns a query to get projects for a specific team and user
func buildTeamProjectsQuery() string {
	return `
query GetTeamProjects($teamId: ID!, $userId: ID!) {
  projects(
    first: 100
    filter: {
      and: [
        { accessibleTeams: { some: { id: { eq: $teamId } } } }
        { members: { some: { id: { eq: $userId } } } }
        { status: { type: { neq: "completed" } } }
      ]
    }
  ) {
    nodes {
      id
      name
      status {
        name
      }
    }
  }
}
`
}

// buildProjectIssuesQuery returns a query to get all issues for a specific project
func buildProjectIssuesQuery() string {
	return `
query GetProjectIssues($projectId: String!) {
  project(id: $projectId) {
    id
    name
    issues(first: 100) {
      nodes {
        id
        identifier
        title
        description
        priority
        state {
          name
          type
        }
        assignee {
          name
        }
        team {
          name
          key
        }
        url
        updatedAt
      }
    }
  }
}
`
}

// buildCreateIssueMutation returns a mutation to create an issue
func buildCreateIssueMutation() string {
	return `
mutation CreateIssue($teamId: String!, $title: String!, $description: String, $priority: Int, $projectId: String) {
  issueCreate(
    input: {
      teamId: $teamId
      title: $title
      description: $description
      priority: $priority
      projectId: $projectId
    }
  ) {
    success
    issue {
      id
      identifier
      title
      url
    }
  }
}
`
}

// buildWorkflowStatesQuery returns a query to get workflow states for a team
func buildWorkflowStatesQuery() string {
	return `
query GetWorkflowStates($teamId: String!) {
  team(id: $teamId) {
    states {
      nodes {
        id
        name
        type
      }
    }
  }
}
`
}

// buildUpdateIssueStateMutation returns a mutation to update an issue's state
func buildUpdateIssueStateMutation() string {
	return `
mutation UpdateIssueState($id: String!, $stateId: String!) {
  issueUpdate(
    id: $id
    input: {
      stateId: $stateId
    }
  ) {
    success
    issue {
      id
      identifier
      title
      state {
        id
        name
        type
      }
    }
  }
}
`
}

// buildAssignIssueMutation returns a mutation to assign an issue to a user
func buildAssignIssueMutation() string {
	return `
mutation AssignIssue($id: String!, $assigneeId: String!) {
  issueUpdate(
    id: $id
    input: {
      assigneeId: $assigneeId
    }
  ) {
    success
    issue {
      id
      identifier
      title
      assignee {
        id
        name
        email
      }
    }
  }
}
`
}

// buildUnassignIssueMutation returns a mutation to unassign an issue
func buildUnassignIssueMutation() string {
	return `
mutation UnassignIssue($id: String!) {
  issueUpdate(
    id: $id
    input: {
      assigneeId: null
    }
  ) {
    success
    issue {
      id
      identifier
      title
      assignee {
        id
        name
        email
      }
    }
  }
}
`
}

// buildUpdateIssueDescriptionMutation returns a mutation to update an issue's description
func buildUpdateIssueDescriptionMutation() string {
	return `
mutation UpdateIssueDescription($id: String!, $description: String!) {
  issueUpdate(
    id: $id
    input: {
      description: $description
    }
  ) {
    success
    issue {
      id
      identifier
      title
      description
    }
  }
}
`
}

// buildUpdateCommentMutation returns a mutation to update a comment
func buildUpdateCommentMutation() string {
	return `
mutation UpdateComment($id: String!, $body: String!) {
  commentUpdate(
    id: $id
    input: {
      body: $body
    }
  ) {
    success
    comment {
      id
      body
      createdAt
      editedAt
      user {
        name
      }
      issue {
        identifier
      }
      url
    }
  }
}
`
}

// buildTeamMembersQuery returns a query to get team members
func buildTeamMembersQuery() string {
	return `
query GetTeamMembers($teamId: String!) {
  team(id: $teamId) {
    members {
      nodes {
        id
        name
        email
      }
    }
  }
}
`
}

// Response Parser Functions

// parseViewerIDResponse parses a viewer ID query response
func parseViewerIDResponse(data json.RawMessage) (string, error) {
	var viewerResp struct {
		Viewer struct {
			ID string `json:"id"`
		} `json:"viewer"`
	}
	if err := json.Unmarshal(data, &viewerResp); err != nil {
		return "", fmt.Errorf("failed to unmarshal viewer data: %w", err)
	}
	return viewerResp.Viewer.ID, nil
}

// parseMyIssuesResponse parses a my issues query response
func parseMyIssuesResponse(data json.RawMessage) ([]Issue, error) {
	var viewerResp ViewerResponse
	if err := json.Unmarshal(data, &viewerResp); err != nil {
		return nil, fmt.Errorf("failed to unmarshal data: %w", err)
	}
	return viewerResp.Viewer.AssignedIssues.Nodes, nil
}

// parseProjectsResponse parses a projects query response
func parseProjectsResponse(data json.RawMessage) ([]Project, error) {
	var projectsResp ProjectsResponse
	if err := json.Unmarshal(data, &projectsResp); err != nil {
		return nil, fmt.Errorf("failed to unmarshal data: %w", err)
	}
	return projectsResp.Projects.Nodes, nil
}

// parseMyTeamsResponse parses a my teams query response
func parseMyTeamsResponse(data json.RawMessage) ([]Team, error) {
	var teamsResp struct {
		Viewer struct {
			Teams struct {
				Nodes []Team `json:"nodes"`
			} `json:"teams"`
		} `json:"viewer"`
	}
	if err := json.Unmarshal(data, &teamsResp); err != nil {
		return nil, fmt.Errorf("failed to unmarshal data: %w", err)
	}
	return teamsResp.Viewer.Teams.Nodes, nil
}

// parseTemplatesResponse parses a templates query response
func parseTemplatesResponse(data json.RawMessage) ([]Template, error) {
	var templatesResp struct {
		Templates []Template `json:"templates"`
	}
	if err := json.Unmarshal(data, &templatesResp); err != nil {
		return nil, fmt.Errorf("failed to unmarshal templates data: %w", err)
	}
	return templatesResp.Templates, nil
}

// parseIssueResponse parses a get issue query response
func parseIssueResponse(data json.RawMessage) (*Issue, error) {
	var issueResp struct {
		Issue Issue `json:"issue"`
	}
	if err := json.Unmarshal(data, &issueResp); err != nil {
		return nil, fmt.Errorf("failed to unmarshal data: %w", err)
	}
	return &issueResp.Issue, nil
}

// parseTemplateResponse parses a get template query response
func parseTemplateResponse(data json.RawMessage) (*Template, error) {
	var templateResp struct {
		Template Template `json:"template"`
	}
	if err := json.Unmarshal(data, &templateResp); err != nil {
		return nil, fmt.Errorf("failed to unmarshal data: %w", err)
	}
	return &templateResp.Template, nil
}

// parseTeamTemplatesResponse parses a team templates query response
func parseTeamTemplatesResponse(data json.RawMessage) ([]Template, error) {
	var teamResp struct {
		Team struct {
			Templates struct {
				Nodes []Template `json:"nodes"`
			} `json:"templates"`
		} `json:"team"`
	}
	if err := json.Unmarshal(data, &teamResp); err != nil {
		return nil, fmt.Errorf("failed to unmarshal data: %w", err)
	}
	return teamResp.Team.Templates.Nodes, nil
}

// parseCreateIssueResponse parses a create issue mutation response
func parseCreateIssueResponse(data json.RawMessage) (bool, *struct {
	ID         string `json:"id"`
	Identifier string `json:"identifier"`
	Title      string `json:"title"`
	URL        string `json:"url"`
}, error) {
	var createResp struct {
		IssueCreate struct {
			Success bool `json:"success"`
			Issue   struct {
				ID         string `json:"id"`
				Identifier string `json:"identifier"`
				Title      string `json:"title"`
				URL        string `json:"url"`
			} `json:"issue"`
		} `json:"issueCreate"`
	}
	if err := json.Unmarshal(data, &createResp); err != nil {
		return false, nil, fmt.Errorf("failed to unmarshal data: %w", err)
	}
	return createResp.IssueCreate.Success, &createResp.IssueCreate.Issue, nil
}

// parseProjectIssuesResponse parses a project issues query response
func parseProjectIssuesResponse(data json.RawMessage) (string, []Issue, error) {
	var projectResp struct {
		Project struct {
			ID     string `json:"id"`
			Name   string `json:"name"`
			Issues struct {
				Nodes []Issue `json:"nodes"`
			} `json:"issues"`
		} `json:"project"`
	}
	if err := json.Unmarshal(data, &projectResp); err != nil {
		return "", nil, fmt.Errorf("failed to unmarshal data: %w", err)
	}
	return projectResp.Project.Name, projectResp.Project.Issues.Nodes, nil
}

// parseWorkflowStatesResponse parses a workflow states query response
func parseWorkflowStatesResponse(data json.RawMessage) ([]WorkflowState, error) {
	var statesResp struct {
		Team struct {
			States struct {
				Nodes []WorkflowState `json:"nodes"`
			} `json:"states"`
		} `json:"team"`
	}
	if err := json.Unmarshal(data, &statesResp); err != nil {
		return nil, fmt.Errorf("failed to unmarshal data: %w", err)
	}
	return statesResp.Team.States.Nodes, nil
}

// parseUpdateIssueStateResponse parses an update issue state mutation response
func parseUpdateIssueStateResponse(data json.RawMessage) (bool, *Issue, error) {
	var updateResp struct {
		IssueUpdate struct {
			Success bool  `json:"success"`
			Issue   Issue `json:"issue"`
		} `json:"issueUpdate"`
	}
	if err := json.Unmarshal(data, &updateResp); err != nil {
		return false, nil, fmt.Errorf("failed to unmarshal data: %w", err)
	}
	return updateResp.IssueUpdate.Success, &updateResp.IssueUpdate.Issue, nil
}

// parseAssignIssueResponse parses an assign issue mutation response
func parseAssignIssueResponse(data json.RawMessage) (bool, *Issue, error) {
	var updateResp struct {
		IssueUpdate struct {
			Success bool  `json:"success"`
			Issue   Issue `json:"issue"`
		} `json:"issueUpdate"`
	}
	if err := json.Unmarshal(data, &updateResp); err != nil {
		return false, nil, fmt.Errorf("failed to unmarshal data: %w", err)
	}
	return updateResp.IssueUpdate.Success, &updateResp.IssueUpdate.Issue, nil
}

// parseTeamMembersResponse parses a team members query response
func parseTeamMembersResponse(data json.RawMessage) ([]User, error) {
	var membersResp struct {
		Team struct {
			Members struct {
				Nodes []User `json:"nodes"`
			} `json:"members"`
		} `json:"team"`
	}
	if err := json.Unmarshal(data, &membersResp); err != nil {
		return nil, fmt.Errorf("failed to unmarshal data: %w", err)
	}
	return membersResp.Team.Members.Nodes, nil
}

// parseUpdateIssueDescriptionResponse parses an update issue description mutation response
func parseUpdateIssueDescriptionResponse(data json.RawMessage) (bool, *Issue, error) {
	var updateResp struct {
		IssueUpdate struct {
			Success bool  `json:"success"`
			Issue   Issue `json:"issue"`
		} `json:"issueUpdate"`
	}
	if err := json.Unmarshal(data, &updateResp); err != nil {
		return false, nil, fmt.Errorf("failed to unmarshal data: %w", err)
	}
	return updateResp.IssueUpdate.Success, &updateResp.IssueUpdate.Issue, nil
}

// UpdatedComment represents a comment that has been updated
type UpdatedComment struct {
	ID        string     `json:"id"`
	Body      string     `json:"body"`
	CreatedAt time.Time  `json:"createdAt"`
	EditedAt  *time.Time `json:"editedAt"`
	User      struct {
		Name string `json:"name"`
	} `json:"user"`
	Issue struct {
		Identifier string `json:"identifier"`
	} `json:"issue"`
	URL string `json:"url"`
}

// parseUpdateCommentResponse parses an update comment mutation response
func parseUpdateCommentResponse(data json.RawMessage) (bool, *UpdatedComment, error) {
	var updateResp struct {
		CommentUpdate struct {
			Success bool           `json:"success"`
			Comment UpdatedComment `json:"comment"`
		} `json:"commentUpdate"`
	}
	if err := json.Unmarshal(data, &updateResp); err != nil {
		return false, nil, fmt.Errorf("failed to unmarshal data: %w", err)
	}
	return updateResp.CommentUpdate.Success, &updateResp.CommentUpdate.Comment, nil
}

type ViewerResponse struct {
	Viewer struct {
		ID             string `json:"id"`
		Name           string `json:"name"`
		Email          string `json:"email"`
		AssignedIssues struct {
			Nodes []Issue `json:"nodes"`
		} `json:"assignedIssues"`
	} `json:"viewer"`
}

type ProjectsResponse struct {
	Projects struct {
		Nodes []Project `json:"nodes"`
	} `json:"projects"`
}

type Project struct {
	ID          string    `json:"id"`
	Name        string    `json:"name"`
	Description string    `json:"description"`
	Progress    float64   `json:"progress"`
	URL         string    `json:"url"`
	Priority    int       `json:"priority"`
	CreatedAt   time.Time `json:"createdAt"`
	UpdatedAt   time.Time `json:"updatedAt"`
	Status      struct {
		Type string `json:"type"`
		Name string `json:"name"`
	} `json:"status"`
	Lead *struct {
		Name string `json:"name"`
	} `json:"lead"`
}

type Issue struct {
	ID          string    `json:"id"`
	Identifier  string    `json:"identifier"`
	Title       string    `json:"title"`
	Description string    `json:"description"`
	Priority    int       `json:"priority"`
	Estimate    *float64  `json:"estimate"`
	URL         string    `json:"url"`
	CreatedAt   time.Time `json:"createdAt"`
	UpdatedAt   time.Time `json:"updatedAt"`
	CompletedAt time.Time `json:"completedAt"`
	State       struct {
		Name string `json:"name"`
		Type string `json:"type"`
	} `json:"state"`
	Assignee *struct {
		Name  string `json:"name"`
		Email string `json:"email"`
	} `json:"assignee"`
	Creator *struct {
		Name string `json:"name"`
	} `json:"creator"`
	Team struct {
		ID   string `json:"id"`
		Name string `json:"name"`
		Key  string `json:"key"`
	} `json:"team"`
	Labels struct {
		Nodes []struct {
			Name  string `json:"name"`
			Color string `json:"color"`
		} `json:"nodes"`
	} `json:"labels"`
	Comments struct {
		Nodes []struct {
			ID        string    `json:"id"`
			Body      string    `json:"body"`
			CreatedAt time.Time `json:"createdAt"`
			User      struct {
				Name string `json:"name"`
			} `json:"user"`
		} `json:"nodes"`
	} `json:"comments"`
}

type Template struct {
	ID           string          `json:"id"`
	Name         string          `json:"name"`
	Description  string          `json:"description"`
	Type         string          `json:"type"`
	CreatedAt    time.Time       `json:"createdAt"`
	TemplateData json.RawMessage `json:"templateData"`
	Team         *struct {
		ID   string `json:"id"`
		Name string `json:"name"`
		Key  string `json:"key"`
	} `json:"team"`
}

type Team struct {
	ID   string `json:"id"`
	Name string `json:"name"`
	Key  string `json:"key"`
}

type WorkflowState struct {
	ID   string `json:"id"`
	Name string `json:"name"`
	Type string `json:"type"`
}

type User struct {
	ID    string `json:"id"`
	Name  string `json:"name"`
	Email string `json:"email"`
}

type TeamsTemplatesResponse struct {
	Viewer struct {
		Teams struct {
			Nodes []struct {
				ID        string `json:"id"`
				Name      string `json:"name"`
				Key       string `json:"key"`
				Templates struct {
					Nodes []Template `json:"nodes"`
				} `json:"templates"`
			} `json:"nodes"`
		} `json:"teams"`
	} `json:"viewer"`
}

func main() {
	apiKey := os.Getenv("LINEAR_API_KEY")
	if apiKey == "" {
		fmt.Fprintln(os.Stderr, "Error: LINEAR_API_KEY environment variable not set")
		os.Exit(1)
	}

	if len(os.Args) > 1 && os.Args[1] == "projects" {
		showAll := false
		jsonOutput := hasJSONFlag()
		for i := 2; i < len(os.Args); i++ {
			if os.Args[i] == "--all" {
				showAll = true
			}
		}

		projects, err := getProjects(apiKey, showAll)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error fetching projects: %v\n", err)
			os.Exit(1)
		}

		if jsonOutput {
			if err := outputJSON(projects); err != nil {
				fmt.Fprintf(os.Stderr, "Error encoding JSON: %v\n", err)
				os.Exit(1)
			}
		} else {
			printProjects(projects)
		}
		return
	}

	if len(os.Args) > 1 && os.Args[1] == "templates" {
		jsonOutput := hasJSONFlag()

		templates, err := getTemplates(apiKey)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error fetching templates: %v\n", err)
			os.Exit(1)
		}

		if jsonOutput {
			if err := outputJSON(templates); err != nil {
				fmt.Fprintf(os.Stderr, "Error encoding JSON: %v\n", err)
				os.Exit(1)
			}
		} else {
			printTemplates(templates)
		}
		return
	}

	if len(os.Args) > 1 && os.Args[1] == "teams" {
		jsonOutput := hasJSONFlag()

		teams, err := getMyTeams(apiKey)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error fetching teams: %v\n", err)
			os.Exit(1)
		}

		if jsonOutput {
			if err := outputJSON(teams); err != nil {
				fmt.Fprintf(os.Stderr, "Error encoding JSON: %v\n", err)
				os.Exit(1)
			}
		} else {
			printTeams(teams)
		}
		return
	}

	if len(os.Args) > 1 && os.Args[1] == "states" {
		if len(os.Args) < 3 {
			fmt.Fprintln(os.Stderr, "Usage: linear-cli states <team-id> [--json]")
			os.Exit(1)
		}
		teamID := os.Args[2]
		jsonOutput := hasJSONFlag()

		states, err := getWorkflowStates(apiKey, teamID)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error fetching workflow states: %v\n", err)
			os.Exit(1)
		}

		if jsonOutput {
			if err := outputJSON(states); err != nil {
				fmt.Fprintf(os.Stderr, "Error encoding JSON: %v\n", err)
				os.Exit(1)
			}
		} else {
			printWorkflowStates(states)
		}
		return
	}

	if len(os.Args) > 1 && os.Args[1] == "create" {
		if err := interactiveCreateIssue(apiKey); err != nil {
			fmt.Fprintf(os.Stderr, "Error creating issue: %v\n", err)
			os.Exit(1)
		}
		return
	}

	if len(os.Args) > 1 && os.Args[1] == "update-state" {
		if len(os.Args) < 4 {
			fmt.Fprintln(os.Stderr, "Usage: linear-cli update-state <issue-id> <state-id> [--json]")
			os.Exit(1)
		}
		issueID := os.Args[2]
		stateID := os.Args[3]
		jsonOutput := hasJSONFlag()

		issue, err := updateIssueState(apiKey, issueID, stateID)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error updating issue state: %v\n", err)
			os.Exit(1)
		}

		if jsonOutput {
			if err := outputJSON(issue); err != nil {
				fmt.Fprintf(os.Stderr, "Error encoding JSON: %v\n", err)
				os.Exit(1)
			}
		} else {
			fmt.Printf("Issue state updated successfully\n")
			fmt.Printf("  Issue: %s\n", issue.Identifier)
			fmt.Printf("  Title: %s\n", issue.Title)
			fmt.Printf("  New State: %s (%s)\n", issue.State.Name, issue.State.Type)
		}
		return
	}

	if len(os.Args) > 1 && os.Args[1] == "assign" {
		if len(os.Args) < 4 {
			fmt.Fprintln(os.Stderr, "Usage: linear-cli assign <issue-id> <user-id> [--json]")
			os.Exit(1)
		}
		issueID := os.Args[2]
		userID := os.Args[3]
		jsonOutput := hasJSONFlag()

		issue, err := assignIssue(apiKey, issueID, userID)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error assigning issue: %v\n", err)
			os.Exit(1)
		}

		if jsonOutput {
			if err := outputJSON(issue); err != nil {
				fmt.Fprintf(os.Stderr, "Error encoding JSON: %v\n", err)
				os.Exit(1)
			}
		} else {
			fmt.Printf("Issue assigned successfully\n")
			fmt.Printf("  Issue: %s\n", issue.Identifier)
			fmt.Printf("  Title: %s\n", issue.Title)
			if issue.Assignee != nil {
				fmt.Printf("  Assigned to: %s (%s)\n", issue.Assignee.Name, issue.Assignee.Email)
			}
		}
		return
	}

	if len(os.Args) > 1 && os.Args[1] == "unassign" {
		if len(os.Args) < 3 {
			fmt.Fprintln(os.Stderr, "Usage: linear-cli unassign <issue-id> [--json]")
			os.Exit(1)
		}
		issueID := os.Args[2]
		jsonOutput := hasJSONFlag()

		issue, err := unassignIssue(apiKey, issueID)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error unassigning issue: %v\n", err)
			os.Exit(1)
		}

		if jsonOutput {
			if err := outputJSON(issue); err != nil {
				fmt.Fprintf(os.Stderr, "Error encoding JSON: %v\n", err)
				os.Exit(1)
			}
		} else {
			fmt.Printf("Issue unassigned successfully\n")
			fmt.Printf("  Issue: %s\n", issue.Identifier)
			fmt.Printf("  Title: %s\n", issue.Title)
			fmt.Printf("  Assignee: (none)\n")
		}
		return
	}

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

	if len(os.Args) > 1 && os.Args[1] == "team-members" {
		if len(os.Args) < 3 {
			fmt.Fprintln(os.Stderr, "Usage: linear-cli team-members <team-id> [--json]")
			os.Exit(1)
		}
		teamID := os.Args[2]
		jsonOutput := hasJSONFlag()

		members, err := getTeamMembers(apiKey, teamID)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error fetching team members: %v\n", err)
			os.Exit(1)
		}

		if jsonOutput {
			if err := outputJSON(members); err != nil {
				fmt.Fprintf(os.Stderr, "Error encoding JSON: %v\n", err)
				os.Exit(1)
			}
		} else {
			printTeamMembers(members)
		}
		return
	}

	if len(os.Args) > 1 && os.Args[1] == "get" {
		if len(os.Args) < 4 {
			fmt.Fprintln(os.Stderr, "Usage: go run main.go get <template|issue> <id> [--json]")
			os.Exit(1)
		}
		resourceType := os.Args[2]
		resourceID := os.Args[3]
		jsonOutput := hasJSONFlag()

		if resourceType == "template" {
			template, err := getTemplate(apiKey, resourceID)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Error fetching template: %v\n", err)
				os.Exit(1)
			}
			if jsonOutput {
				if err := outputJSON(template); err != nil {
					fmt.Fprintf(os.Stderr, "Error encoding JSON: %v\n", err)
					os.Exit(1)
				}
			} else {
				printTemplateDetails(template)
			}
		} else if resourceType == "issue" {
			issue, err := getIssue(apiKey, resourceID)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Error fetching issue: %v\n", err)
				os.Exit(1)
			}
			if jsonOutput {
				if err := outputJSON(issue); err != nil {
					fmt.Fprintf(os.Stderr, "Error encoding JSON: %v\n", err)
					os.Exit(1)
				}
			} else {
				printIssueDetails(issue)
			}
		} else {
			fmt.Fprintf(os.Stderr, "Unknown resource type: %s (must be 'template' or 'issue')\n", resourceType)
			os.Exit(1)
		}
		return
	}


	if len(os.Args) > 1 && (os.Args[1] == "comment" || os.Args[1] == "add-comment") {
		if len(os.Args) < 4 {
			fmt.Fprintln(os.Stderr, "Usage: linear-cli comment <issue-id> <comment-text> [--json]")
			fmt.Fprintln(os.Stderr, "       linear-cli add-comment <issue-id> <comment-text> [--json]")
			os.Exit(1)
		}
		issueID := os.Args[2]
		commentBody := strings.Join(os.Args[3:], " ")

		// Remove --json flag from comment body if present
		commentBody = strings.TrimSuffix(commentBody, " --json")
		commentBody = strings.TrimSpace(commentBody)

		if commentBody == "" {
			fmt.Fprintln(os.Stderr, "Error: comment text cannot be empty")
			os.Exit(1)
		}

		jsonOutput := hasJSONFlag()

		comment, err := addComment(apiKey, issueID, commentBody)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error adding comment: %v\n", err)
			os.Exit(1)
		}

		if jsonOutput {
			if err := outputJSON(comment); err != nil {
				fmt.Fprintf(os.Stderr, "Error encoding JSON: %v\n", err)
				os.Exit(1)
			}
		} else {
			fmt.Printf("Comment added successfully\n")
			fmt.Printf("  Comment ID: %s\n", comment.ID)
			fmt.Printf("  Issue: %s\n", comment.Issue.Identifier)
			fmt.Printf("  URL: %s\n", comment.URL)
		}
		return
	}

	if len(os.Args) > 1 && os.Args[1] == "update-description" {
		if len(os.Args) < 4 {
			fmt.Fprintln(os.Stderr, "Usage: linear-cli update-description <issue-id> <description-text> [--json]")
			os.Exit(1)
		}
		issueID := os.Args[2]
		descriptionText := strings.Join(os.Args[3:], " ")

		// Remove --json flag from description text if present
		descriptionText = strings.TrimSuffix(descriptionText, " --json")
		descriptionText = strings.TrimSpace(descriptionText)

		if descriptionText == "" {
			fmt.Fprintln(os.Stderr, "Error: description text cannot be empty")
			os.Exit(1)
		}

		jsonOutput := hasJSONFlag()

		issue, err := updateIssueDescription(apiKey, issueID, descriptionText)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error updating issue description: %v\n", err)
			os.Exit(1)
		}

		if jsonOutput {
			if err := outputJSON(issue); err != nil {
				fmt.Fprintf(os.Stderr, "Error encoding JSON: %v\n", err)
				os.Exit(1)
			}
		} else {
			fmt.Printf("Issue description updated successfully\n")
			fmt.Printf("  Issue: %s\n", issue.Identifier)
			fmt.Printf("  Title: %s\n", issue.Title)
			fmt.Printf("  Description: %s\n", issue.Description)
		}
		return
	}

	if len(os.Args) > 1 && os.Args[1] == "update-comment" {
		if len(os.Args) < 4 {
			fmt.Fprintln(os.Stderr, "Usage: linear-cli update-comment <comment-id> <comment-text> [--json]")
			os.Exit(1)
		}
		commentID := os.Args[2]
		commentBody := strings.Join(os.Args[3:], " ")

		// Remove --json flag from comment body if present
		commentBody = strings.TrimSuffix(commentBody, " --json")
		commentBody = strings.TrimSpace(commentBody)

		if commentBody == "" {
			fmt.Fprintln(os.Stderr, "Error: comment text cannot be empty")
			os.Exit(1)
		}

		jsonOutput := hasJSONFlag()

		comment, err := updateComment(apiKey, commentID, commentBody)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error updating comment: %v\n", err)
			os.Exit(1)
		}

		if jsonOutput {
			if err := outputJSON(comment); err != nil {
				fmt.Fprintf(os.Stderr, "Error encoding JSON: %v\n", err)
				os.Exit(1)
			}
		} else {
			fmt.Printf("Comment updated successfully\n")
			fmt.Printf("  Comment ID: %s\n", comment.ID)
			fmt.Printf("  Issue: %s\n", comment.Issue.Identifier)
			fmt.Printf("  Body: %s\n", comment.Body)
			if comment.EditedAt != nil {
				fmt.Printf("  Edited At: %s\n", comment.EditedAt.Format("2006-01-02 15:04"))
			}
			fmt.Printf("  URL: %s\n", comment.URL)
		}
		return
	}

	if len(os.Args) > 2 && (os.Args[1] == "issues" || os.Args[1] == "project-issues") {
		projectID := os.Args[2]
		jsonOutput := hasJSONFlag()

		projectName, issues, err := getProjectIssues(apiKey, projectID)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error fetching project issues: %v\n", err)
			os.Exit(1)
		}

		if jsonOutput {
			if err := outputJSON(issues); err != nil {
				fmt.Fprintf(os.Stderr, "Error encoding JSON: %v\n", err)
				os.Exit(1)
			}
		} else {
			printProjectIssues(projectName, issues)
		}
		return
	}

	// Default command: list issues
	jsonOutput := hasJSONFlag()

	issues, err := getMyIssues(apiKey)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error fetching issues: %v\n", err)
		os.Exit(1)
	}

	if jsonOutput {
		if err := outputJSON(issues); err != nil {
			fmt.Fprintf(os.Stderr, "Error encoding JSON: %v\n", err)
			os.Exit(1)
		}
	} else {
		printIssues(issues)
	}
}

func getMyIssues(apiKey string) ([]Issue, error) {
	data, err := executeGraphQL(apiKey, buildMyIssuesQuery(), nil)
	if err != nil {
		return nil, err
	}
	return parseMyIssuesResponse(data)
}

func printIssues(issues []Issue) {
	if len(issues) == 0 {
		fmt.Println("No issues assigned to you")
		return
	}

	fmt.Printf("Found %d issue(s):\n\n", len(issues))

	for _, issue := range issues {
		fmt.Printf("[%s] %s\n", issue.Identifier, issue.Title)
		fmt.Printf("  State: %s (%s)\n", issue.State.Name, issue.State.Type)
		fmt.Printf("  Team: %s (%s)\n", issue.Team.Name, issue.Team.Key)
		fmt.Printf("  Priority: %s\n", priorityLabel(issue.Priority))
		if issue.Description != "" {
			desc := issue.Description
			if len(desc) > 100 {
				desc = desc[:100] + "..."
			}
			fmt.Printf("  Description: %s\n", desc)
		}
		fmt.Printf("  URL: %s\n", issue.URL)
		fmt.Printf("  Updated: %s\n", issue.UpdatedAt.Format("2006-01-02 15:04"))
		fmt.Println()
	}
}

func getProjects(apiKey string, showAll bool) ([]Project, error) {
	var viewerID string
	if !showAll {
		data, err := executeGraphQL(apiKey, buildViewerIDQuery(), nil)
		if err != nil {
			return nil, err
		}
		viewerID, err = parseViewerIDResponse(data)
		if err != nil {
			return nil, err
		}
	}

	query := buildProjectsQuery(viewerID, !showAll)
	data, err := executeGraphQL(apiKey, query, nil)
	if err != nil {
		return nil, err
	}
	return parseProjectsResponse(data)
}

func printProjects(projects []Project) {
	if len(projects) == 0 {
		fmt.Println("No active projects")
		return
	}

	fmt.Printf("Found %d project(s):\n\n", len(projects))

	for _, proj := range projects {
		fmt.Printf("%s\n", proj.Name)
		fmt.Printf("  Status: %s (%s)\n", proj.Status.Name, proj.Status.Type)
		fmt.Printf("  Progress: %.0f%%\n", proj.Progress*100)
		fmt.Printf("  Priority: %s\n", priorityLabel(proj.Priority))
		if proj.Lead != nil {
			fmt.Printf("  Lead: %s\n", proj.Lead.Name)
		}
		if proj.Description != "" {
			desc := proj.Description
			if len(desc) > 100 {
				desc = desc[:100] + "..."
			}
			fmt.Printf("  Description: %s\n", desc)
		}
		fmt.Printf("  URL: %s\n", proj.URL)
		fmt.Printf("  Updated: %s\n", proj.UpdatedAt.Format("2006-01-02 15:04"))
		fmt.Println()
	}
}

func getTemplates(apiKey string) (map[string][]Template, error) {
	// Get user's teams
	teamsData, err := executeGraphQL(apiKey, buildMyTeamsQuery(), nil)
	if err != nil {
		return nil, err
	}
	teams, err := parseMyTeamsResponse(teamsData)
	if err != nil {
		return nil, err
	}

	// Build team ID map
	myTeamIDs := make(map[string]string) // ID -> Name
	for _, team := range teams {
		myTeamIDs[team.ID] = team.Name
	}

	// Get all templates
	templatesData, err := executeGraphQL(apiKey, buildTemplatesQuery(), nil)
	if err != nil {
		return nil, err
	}
	templates, err := parseTemplatesResponse(templatesData)
	if err != nil {
		return nil, err
	}

	// Organize templates by team, filtering to only user's teams
	templatesByTeam := make(map[string][]Template)
	for _, tmpl := range templates {
		if tmpl.Team == nil {
			templatesByTeam["Workspace"] = append(templatesByTeam["Workspace"], tmpl)
			continue
		}
		if teamName, isMember := myTeamIDs[tmpl.Team.ID]; isMember {
			templatesByTeam[teamName] = append(templatesByTeam[teamName], tmpl)
		}
	}

	return templatesByTeam, nil
}

func printTemplates(templatesByTeam map[string][]Template) {
	if len(templatesByTeam) == 0 {
		fmt.Println("No templates found for your teams")
		return
	}

	totalCount := 0
	for _, templates := range templatesByTeam {
		totalCount += len(templates)
	}

	fmt.Printf("Found %d template(s) across %d team(s):\n\n", totalCount, len(templatesByTeam))

	for teamName, templates := range templatesByTeam {
		fmt.Printf("Team: %s (%d template(s))\n", teamName, len(templates))
		for _, tmpl := range templates {
			fmt.Printf("  - %s", tmpl.Name)
			if tmpl.Type != "" {
				fmt.Printf(" [%s]", tmpl.Type)
			}
			fmt.Printf(" (ID: %s)", tmpl.ID)
			fmt.Println()
			if tmpl.Description != "" {
				desc := tmpl.Description
				if len(desc) > 80 {
					desc = desc[:80] + "..."
				}
				fmt.Printf("    %s\n", desc)
			}
		}
		fmt.Println()
	}
}

func interactiveCreateIssue(apiKey string) error {
	reader := bufio.NewReader(os.Stdin)

	// Get user's teams
	teams, err := getMyTeams(apiKey)
	if err != nil {
		return fmt.Errorf("failed to get teams: %w", err)
	}

	if len(teams) == 0 {
		return fmt.Errorf("you are not a member of any teams")
	}

	// Select team
	fmt.Println("Select a team:")
	for i, team := range teams {
		fmt.Printf("%d. %s (%s)\n", i+1, team.Name, team.Key)
	}
	fmt.Print("Team number: ")
	teamNumStr, _ := reader.ReadString('\n')
	teamNum, err := strconv.Atoi(strings.TrimSpace(teamNumStr))
	if err != nil || teamNum < 1 || teamNum > len(teams) {
		return fmt.Errorf("invalid team selection")
	}
	selectedTeam := teams[teamNum-1]

	// Get templates for selected team
	templates, err := getTeamTemplates(apiKey, selectedTeam.ID)
	if err != nil {
		return fmt.Errorf("failed to get templates: %w", err)
	}

	// Select template (optional)
	var selectedTemplate *Template
	if len(templates) > 0 {
		fmt.Println("\nSelect a template (or 0 for none):")
		fmt.Println("0. No template")
		for i, tmpl := range templates {
			fmt.Printf("%d. %s\n", i+1, tmpl.Name)
		}
		fmt.Print("Template number: ")
		tmplNumStr, _ := reader.ReadString('\n')
		tmplNum, err := strconv.Atoi(strings.TrimSpace(tmplNumStr))
		if err != nil || tmplNum < 0 || tmplNum > len(templates) {
			return fmt.Errorf("invalid template selection")
		}
		if tmplNum > 0 {
			selectedTemplate = &templates[tmplNum-1]
		}
	}

	// Extract template data
	var templateTitle, templateDescription string
	var templatePriority int
	if selectedTemplate != nil && len(selectedTemplate.TemplateData) > 0 {
		var tmplData map[string]interface{}
		if err := json.Unmarshal(selectedTemplate.TemplateData, &tmplData); err == nil {
			if title, ok := tmplData["title"].(string); ok {
				templateTitle = title
			}
			if desc, ok := tmplData["description"].(string); ok {
				templateDescription = desc
			}
			if priority, ok := tmplData["priority"].(float64); ok {
				templatePriority = int(priority)
			}
		}
	}

	// Get title
	fmt.Print("\nTitle")
	if templateTitle != "" {
		fmt.Printf(" [%s]", templateTitle)
	}
	fmt.Print(": ")
	title, _ := reader.ReadString('\n')
	title = strings.TrimSpace(title)
	if title == "" && templateTitle != "" {
		title = templateTitle
	}
	if title == "" {
		return fmt.Errorf("title is required")
	}

	// Get description
	fmt.Print("Description")
	if templateDescription != "" {
		fmt.Printf(" [%s]", templateDescription)
	}
	fmt.Print(": ")
	description, _ := reader.ReadString('\n')
	description = strings.TrimSpace(description)
	if description == "" && templateDescription != "" {
		description = templateDescription
	}

	// Get priority
	fmt.Print("Priority (0=None, 1=Urgent, 2=High, 3=Normal, 4=Low)")
	if templatePriority > 0 {
		fmt.Printf(" [%d]", templatePriority)
	}
	fmt.Print(": ")
	priorityStr, _ := reader.ReadString('\n')
	priorityStr = strings.TrimSpace(priorityStr)
	priority := 0
	if priorityStr != "" {
		p, err := strconv.Atoi(priorityStr)
		if err == nil && p >= 0 && p <= 4 {
			priority = p
		}
	} else if templatePriority > 0 {
		priority = templatePriority
	}

	// Get user's projects for this team
	projects, err := getUserProjectsForTeam(apiKey, selectedTeam.ID)
	if err != nil {
		return fmt.Errorf("failed to get projects: %w", err)
	}

	// Select project (optional)
	var selectedProjectID *string
	if len(projects) > 0 {
		fmt.Println("\nSelect a project (or 0 for none):")
		fmt.Println("0. No project")
		for i, proj := range projects {
			fmt.Printf("%d. %s\n", i+1, proj.Name)
		}
		fmt.Print("Project number: ")
		projNumStr, _ := reader.ReadString('\n')
		projNum, err := strconv.Atoi(strings.TrimSpace(projNumStr))
		if err != nil || projNum < 0 || projNum > len(projects) {
			return fmt.Errorf("invalid project selection")
		}
		if projNum > 0 {
			selectedProjectID = &projects[projNum-1].ID
		}
	}

	// Create the issue
	variables := map[string]interface{}{
		"teamId":      selectedTeam.ID,
		"title":       title,
		"description": description,
		"priority":    priority,
	}
	if selectedProjectID != nil {
		variables["projectId"] = *selectedProjectID
	}

	data, err := executeGraphQL(apiKey, buildCreateIssueMutation(), variables)
	if err != nil {
		return err
	}

	success, issue, err := parseCreateIssueResponse(data)
	if err != nil {
		return err
	}

	if !success {
		return fmt.Errorf("failed to create issue")
	}

	fmt.Printf("\nIssue created successfully!\n")
	fmt.Printf("  ID: %s\n", issue.Identifier)
	fmt.Printf("  Title: %s\n", issue.Title)
	fmt.Printf("  URL: %s\n", issue.URL)

	return nil
}

func getMyTeams(apiKey string) ([]Team, error) {
	data, err := executeGraphQL(apiKey, buildMyTeamsQuery(), nil)
	if err != nil {
		return nil, err
	}
	return parseMyTeamsResponse(data)
}

func printTeams(teams []Team) {
	if len(teams) == 0 {
		fmt.Println("No teams found")
		return
	}

	fmt.Printf("Found %d team(s):\n\n", len(teams))

	for _, team := range teams {
		fmt.Printf("%s (%s)\n", team.Name, team.Key)
		fmt.Printf("  ID: %s\n", team.ID)
		fmt.Println()
	}
}

func printWorkflowStates(states []WorkflowState) {
	if len(states) == 0 {
		fmt.Println("No workflow states found")
		return
	}

	fmt.Printf("Found %d workflow state(s):\n\n", len(states))

	for _, state := range states {
		fmt.Printf("%s\n", state.Name)
		fmt.Printf("  ID: %s\n", state.ID)
		fmt.Printf("  Type: %s\n", state.Type)
		fmt.Println()
	}
}

func printTeamMembers(members []User) {
	if len(members) == 0 {
		fmt.Println("No team members found")
		return
	}

	fmt.Printf("Found %d team member(s):\n\n", len(members))

	for _, member := range members {
		fmt.Printf("%s\n", member.Name)
		fmt.Printf("  ID: %s\n", member.ID)
		fmt.Printf("  Email: %s\n", member.Email)
		fmt.Println()
	}
}

func getWorkflowStates(apiKey string, teamID string) ([]WorkflowState, error) {
	variables := map[string]interface{}{
		"teamId": teamID,
	}
	data, err := executeGraphQL(apiKey, buildWorkflowStatesQuery(), variables)
	if err != nil {
		return nil, err
	}
	return parseWorkflowStatesResponse(data)
}

func updateIssueState(apiKey string, issueID string, stateID string) (*Issue, error) {
	variables := map[string]interface{}{
		"id":      issueID,
		"stateId": stateID,
	}
	data, err := executeGraphQL(apiKey, buildUpdateIssueStateMutation(), variables)
	if err != nil {
		return nil, err
	}
	success, issue, err := parseUpdateIssueStateResponse(data)
	if err != nil {
		return nil, err
	}
	if !success {
		return nil, fmt.Errorf("failed to update issue state")
	}
	return issue, nil
}

func getTeamMembers(apiKey string, teamID string) ([]User, error) {
	variables := map[string]interface{}{
		"teamId": teamID,
	}
	data, err := executeGraphQL(apiKey, buildTeamMembersQuery(), variables)
	if err != nil {
		return nil, err
	}
	return parseTeamMembersResponse(data)
}

func assignIssue(apiKey string, issueID string, userID string) (*Issue, error) {
	variables := map[string]interface{}{
		"id":         issueID,
		"assigneeId": userID,
	}
	data, err := executeGraphQL(apiKey, buildAssignIssueMutation(), variables)
	if err != nil {
		return nil, err
	}
	success, issue, err := parseAssignIssueResponse(data)
	if err != nil {
		return nil, err
	}
	if !success {
		return nil, fmt.Errorf("failed to assign issue")
	}
	return issue, nil
}

func unassignIssue(apiKey string, issueID string) (*Issue, error) {
	variables := map[string]interface{}{
		"id": issueID,
	}
	data, err := executeGraphQL(apiKey, buildUnassignIssueMutation(), variables)
	if err != nil {
		return nil, err
	}
	success, issue, err := parseAssignIssueResponse(data)
	if err != nil {
		return nil, err
	}
	if !success {
		return nil, fmt.Errorf("failed to unassign issue")
	}
	return issue, nil
}

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

func updateIssueDescription(apiKey string, issueID string, description string) (*Issue, error) {
	variables := map[string]interface{}{
		"id":          issueID,
		"description": description,
	}
	data, err := executeGraphQL(apiKey, buildUpdateIssueDescriptionMutation(), variables)
	if err != nil {
		return nil, err
	}
	success, issue, err := parseUpdateIssueDescriptionResponse(data)
	if err != nil {
		return nil, err
	}
	if !success {
		return nil, fmt.Errorf("failed to update issue description")
	}
	return issue, nil
}

func updateComment(apiKey string, commentID string, body string) (*UpdatedComment, error) {
	variables := map[string]interface{}{
		"id":   commentID,
		"body": body,
	}
	data, err := executeGraphQL(apiKey, buildUpdateCommentMutation(), variables)
	if err != nil {
		return nil, err
	}
	success, comment, err := parseUpdateCommentResponse(data)
	if err != nil {
		return nil, err
	}
	if !success {
		return nil, fmt.Errorf("failed to update comment")
	}
	return comment, nil
}

func getTeamTemplates(apiKey string, teamID string) ([]Template, error) {
	variables := map[string]interface{}{
		"teamId": teamID,
	}
	data, err := executeGraphQL(apiKey, buildTeamTemplatesQuery(), variables)
	if err != nil {
		return nil, err
	}
	return parseTeamTemplatesResponse(data)
}

func getUserProjectsForTeam(apiKey string, teamID string) ([]Project, error) {
	// Get viewer ID
	viewerData, err := executeGraphQL(apiKey, buildViewerIDQuery(), nil)
	if err != nil {
		return nil, err
	}
	viewerID, err := parseViewerIDResponse(viewerData)
	if err != nil {
		return nil, err
	}

	// Get team projects for this user
	variables := map[string]interface{}{
		"teamId": teamID,
		"userId": viewerID,
	}
	data, err := executeGraphQL(apiKey, buildTeamProjectsQuery(), variables)
	if err != nil {
		return nil, err
	}
	return parseProjectsResponse(data)
}

func getTemplate(apiKey string, templateID string) (*Template, error) {
	variables := map[string]interface{}{
		"id": templateID,
	}
	data, err := executeGraphQL(apiKey, buildGetTemplateQuery(), variables)
	if err != nil {
		return nil, err
	}
	return parseTemplateResponse(data)
}

func getIssue(apiKey string, issueID string) (*Issue, error) {
	variables := map[string]interface{}{
		"id": issueID,
	}
	data, err := executeGraphQL(apiKey, buildGetIssueQuery(), variables)
	if err != nil {
		return nil, err
	}
	return parseIssueResponse(data)
}

func printTemplateDetails(tmpl *Template) {
	fmt.Printf("Template: %s\n", tmpl.Name)
	fmt.Printf("ID: %s\n", tmpl.ID)
	fmt.Printf("Type: %s\n", tmpl.Type)
	if tmpl.Team != nil {
		fmt.Printf("Team: %s (%s)\n", tmpl.Team.Name, tmpl.Team.Key)
	} else {
		fmt.Printf("Team: Workspace\n")
	}
	if tmpl.Description != "" {
		fmt.Printf("Description: %s\n", tmpl.Description)
	}
	fmt.Printf("Created: %s\n", tmpl.CreatedAt.Format("2006-01-02 15:04"))

	if len(tmpl.TemplateData) > 0 {
		fmt.Println("\nTemplate Data:")
		var tmplData map[string]interface{}
		if err := json.Unmarshal(tmpl.TemplateData, &tmplData); err == nil {
			prettyData, _ := json.MarshalIndent(tmplData, "  ", "  ")
			fmt.Printf("  %s\n", string(prettyData))
		}
	}
}

func printIssueDetails(issue *Issue) {
	fmt.Printf("[%s] %s\n", issue.Identifier, issue.Title)
	fmt.Printf("ID: %s\n", issue.ID)
	fmt.Printf("URL: %s\n", issue.URL)
	fmt.Printf("Team: %s (%s)\n", issue.Team.Name, issue.Team.Key)
	fmt.Printf("State: %s (%s)\n", issue.State.Name, issue.State.Type)
	fmt.Printf("Priority: %s\n", priorityLabel(issue.Priority))

	if issue.Assignee != nil {
		fmt.Printf("Assignee: %s\n", issue.Assignee.Name)
	}

	fmt.Printf("Created: %s\n", issue.CreatedAt.Format("2006-01-02 15:04"))
	fmt.Printf("Updated: %s\n", issue.UpdatedAt.Format("2006-01-02 15:04"))

	if !issue.CompletedAt.IsZero() {
		fmt.Printf("Completed: %s\n", issue.CompletedAt.Format("2006-01-02 15:04"))
	}

	if issue.Description != "" {
		fmt.Printf("\nDescription:\n%s\n", issue.Description)
	}

	// Print comments if any
	fmt.Printf("\nComments: %d\n", len(issue.Comments.Nodes))
	if len(issue.Comments.Nodes) > 0 {
		for _, comment := range issue.Comments.Nodes {
			fmt.Printf("\n  [%s] %s:\n", comment.CreatedAt.Format("2006-01-02 15:04"), comment.User.Name)
			fmt.Printf("  %s\n", comment.Body)
		}
	}
}

func getProjectIssues(apiKey string, projectID string) (string, []Issue, error) {
	variables := map[string]interface{}{
		"projectId": projectID,
	}
	data, err := executeGraphQL(apiKey, buildProjectIssuesQuery(), variables)
	if err != nil {
		return "", nil, err
	}
	return parseProjectIssuesResponse(data)
}

func printProjectIssues(projectName string, issues []Issue) {
	if len(issues) == 0 {
		fmt.Printf("No issues found in project: %s\n", projectName)
		return
	}

	fmt.Printf("Project: %s\n", projectName)
	fmt.Printf("Found %d issue(s):\n\n", len(issues))

	for _, issue := range issues {
		fmt.Printf("[%s] %s\n", issue.Identifier, issue.Title)
		fmt.Printf("  State: %s (%s)\n", issue.State.Name, issue.State.Type)
		fmt.Printf("  Team: %s (%s)\n", issue.Team.Name, issue.Team.Key)
		fmt.Printf("  Priority: %s\n", priorityLabel(issue.Priority))
		if issue.Assignee != nil {
			fmt.Printf("  Assignee: %s\n", issue.Assignee.Name)
		}
		if issue.Description != "" {
			desc := issue.Description
			if len(desc) > 100 {
				desc = desc[:100] + "..."
			}
			fmt.Printf("  Description: %s\n", desc)
		}
		fmt.Printf("  URL: %s\n", issue.URL)
		fmt.Printf("  Updated: %s\n", issue.UpdatedAt.Format("2006-01-02 15:04"))
		fmt.Println()
	}
}

func priorityLabel(priority int) string {
	switch priority {
	case 0:
		return "None"
	case 1:
		return "Urgent"
	case 2:
		return "High"
	case 3:
		return "Normal"
	case 4:
		return "Low"
	default:
		return fmt.Sprintf("Unknown (%d)", priority)
	}
}
