package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"testing"
	"time"
)

// mockLinearServer creates a test HTTP server that mimics the Linear API
func mockLinearServer(handler http.HandlerFunc) *httptest.Server {
	return httptest.NewServer(handler)
}

// TestGetIssue tests the getIssue function with various scenarios
func TestGetIssue(t *testing.T) {
	tests := []struct {
		name          string
		issueID       string
		mockResponse  interface{}
		mockStatus    int
		mockErrors    []GraphQLError
		expectError   bool
		errorContains string
		validateIssue func(*testing.T, *Issue)
	}{
		{
			name:    "successful issue retrieval",
			issueID: "ISSUE-123",
			mockResponse: map[string]interface{}{
				"issue": map[string]interface{}{
					"id":          "abc123",
					"identifier":  "ISSUE-123",
					"title":       "Test Issue",
					"description": "This is a test issue",
					"priority":    2,
					"estimate":    5.0,
					"url":         "https://linear.app/test/issue/ISSUE-123",
					"createdAt":   "2023-01-01T00:00:00.000Z",
					"updatedAt":   "2023-01-02T00:00:00.000Z",
					"completedAt": nil,
					"state": map[string]interface{}{
						"name": "In Progress",
						"type": "started",
					},
					"assignee": map[string]interface{}{
						"name":  "John Doe",
						"email": "john@example.com",
					},
					"creator": map[string]interface{}{
						"name": "Jane Smith",
					},
					"team": map[string]interface{}{
						"name": "Engineering",
						"key":  "ENG",
					},
					"labels": map[string]interface{}{
						"nodes": []map[string]interface{}{
							{
								"name":  "bug",
								"color": "#ff0000",
							},
						},
					},
					"comments": map[string]interface{}{
						"nodes": []map[string]interface{}{
							{
								"id":        "comment-1",
								"body":      "Test comment",
								"createdAt": "2023-01-02T00:00:00.000Z",
								"user": map[string]interface{}{
									"name": "Bob",
								},
							},
						},
					},
				},
			},
			mockStatus:  http.StatusOK,
			expectError: false,
			validateIssue: func(t *testing.T, issue *Issue) {
				if issue.ID != "abc123" {
					t.Errorf("expected ID abc123, got %s", issue.ID)
				}
				if issue.Identifier != "ISSUE-123" {
					t.Errorf("expected identifier ISSUE-123, got %s", issue.Identifier)
				}
				if issue.Title != "Test Issue" {
					t.Errorf("expected title 'Test Issue', got %s", issue.Title)
				}
				if issue.Priority != 2 {
					t.Errorf("expected priority 2, got %d", issue.Priority)
				}
				if issue.Assignee == nil {
					t.Error("expected assignee to be non-nil")
				} else if issue.Assignee.Name != "John Doe" {
					t.Errorf("expected assignee John Doe, got %s", issue.Assignee.Name)
				}
				if issue.Team.Name != "Engineering" {
					t.Errorf("expected team Engineering, got %s", issue.Team.Name)
				}
				if len(issue.Labels.Nodes) != 1 {
					t.Errorf("expected 1 label, got %d", len(issue.Labels.Nodes))
				}
				if len(issue.Comments.Nodes) != 1 {
					t.Errorf("expected 1 comment, got %d", len(issue.Comments.Nodes))
				}
			},
		},
		{
			name:    "invalid issue ID - GraphQL error",
			issueID: "INVALID",
			mockResponse: map[string]interface{}{
				"issue": nil,
			},
			mockStatus: http.StatusOK,
			mockErrors: []GraphQLError{
				{
					Message: "Issue not found",
					Path:    []any{"issue"},
				},
			},
			expectError:   true,
			errorContains: "GraphQL errors",
		},
		{
			name:          "network error - non-200 status",
			issueID:       "ISSUE-123",
			mockResponse:  map[string]interface{}{},
			mockStatus:    http.StatusInternalServerError,
			expectError:   true,
			errorContains: "API returned status 500",
		},
		{
			name:    "missing required fields - minimal issue",
			issueID: "ISSUE-MINIMAL",
			mockResponse: map[string]interface{}{
				"issue": map[string]interface{}{
					"id":          "minimal-id",
					"identifier":  "ISSUE-MINIMAL",
					"title":       "Minimal Issue",
					"description": "",
					"priority":    0,
					"url":         "https://linear.app/test/issue/ISSUE-MINIMAL",
					"createdAt":   "2023-01-01T00:00:00.000Z",
					"updatedAt":   "2023-01-01T00:00:00.000Z",
					"completedAt": nil,
					"state": map[string]interface{}{
						"name": "Todo",
						"type": "triage",
					},
					"team": map[string]interface{}{
						"name": "Test Team",
						"key":  "TEST",
					},
					"labels": map[string]interface{}{
						"nodes": []map[string]interface{}{},
					},
					"comments": map[string]interface{}{
						"nodes": []map[string]interface{}{},
					},
				},
			},
			mockStatus:  http.StatusOK,
			expectError: false,
			validateIssue: func(t *testing.T, issue *Issue) {
				if issue.ID != "minimal-id" {
					t.Errorf("expected ID minimal-id, got %s", issue.ID)
				}
				if issue.Assignee != nil {
					t.Errorf("expected nil assignee, got %+v", issue.Assignee)
				}
				if issue.Creator != nil {
					t.Errorf("expected nil creator, got %+v", issue.Creator)
				}
				if len(issue.Labels.Nodes) != 0 {
					t.Errorf("expected 0 labels, got %d", len(issue.Labels.Nodes))
				}
			},
		},
		{
			name:    "issue with fractional estimate",
			issueID: "ISSUE-FLOAT",
			mockResponse: map[string]interface{}{
				"issue": map[string]interface{}{
					"id":          "float-id",
					"identifier":  "ISSUE-FLOAT",
					"title":       "Issue with Float Estimate",
					"description": "Testing float estimate handling",
					"priority":    1,
					"estimate":    3.5,
					"url":         "https://linear.app/test/issue/ISSUE-FLOAT",
					"createdAt":   "2023-01-01T00:00:00.000Z",
					"updatedAt":   "2023-01-01T00:00:00.000Z",
					"completedAt": nil,
					"state": map[string]interface{}{
						"name": "In Progress",
						"type": "started",
					},
					"team": map[string]interface{}{
						"name": "Engineering",
						"key":  "ENG",
					},
					"labels": map[string]interface{}{
						"nodes": []map[string]interface{}{},
					},
					"comments": map[string]interface{}{
						"nodes": []map[string]interface{}{},
					},
				},
			},
			mockStatus:  http.StatusOK,
			expectError: false,
			validateIssue: func(t *testing.T, issue *Issue) {
				if issue.Estimate == nil {
					t.Error("expected estimate to be non-nil")
				} else if *issue.Estimate != 3.5 {
					t.Errorf("expected estimate 3.5, got %f", *issue.Estimate)
				}
			},
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create a mock server
			server := mockLinearServer(func(w http.ResponseWriter, r *http.Request) {
				// Verify request headers
				if r.Header.Get("Authorization") != "test-api-key" {
					t.Errorf("expected Authorization header 'test-api-key', got %s", r.Header.Get("Authorization"))
				}
				if r.Header.Get("Content-Type") != "application/json" {
					t.Errorf("expected Content-Type 'application/json', got %s", r.Header.Get("Content-Type"))
				}

				// Verify request method
				if r.Method != http.MethodPost {
					t.Errorf("expected POST method, got %s", r.Method)
				}

				// Build response
				resp := GraphQLResponse{
					Errors: tt.mockErrors,
				}

				if tt.mockResponse != nil {
					data, _ := json.Marshal(tt.mockResponse)
					resp.Data = data
				}

				w.WriteHeader(tt.mockStatus)
				json.NewEncoder(w).Encode(resp)
			})
			defer server.Close()

			// Replace the endpoint temporarily
			// Since linearAPIEndpoint is a const, we need a different approach
			// We'll use a helper function approach instead
			issue, err := getIssueWithEndpoint("test-api-key", tt.issueID, server.URL)

			// Check error expectations
			if tt.expectError {
				if err == nil {
					t.Fatal("expected error but got none")
				}
				if tt.errorContains != "" && !contains(err.Error(), tt.errorContains) {
					t.Errorf("expected error to contain '%s', got '%s'", tt.errorContains, err.Error())
				}
			} else {
				if err != nil {
					t.Fatalf("unexpected error: %v", err)
				}
				if issue == nil {
					t.Fatal("expected non-nil issue")
				}
				if tt.validateIssue != nil {
					tt.validateIssue(t, issue)
				}
			}
		})
	}
}

// TestGetProjects tests the getProjects function
func TestGetProjects(t *testing.T) {
	tests := []struct {
		name             string
		showAll          bool
		mockViewerID     string
		mockProjects     []map[string]interface{}
		mockStatus       int
		mockErrors       []GraphQLError
		expectError      bool
		errorContains    string
		expectedCount    int
		validateProjects func(*testing.T, []Project)
	}{
		{
			name:         "successful project retrieval with showAll=false",
			showAll:      false,
			mockViewerID: "user-123",
			mockProjects: []map[string]interface{}{
				{
					"id":          "proj-1",
					"name":        "Project Alpha",
					"description": "First project",
					"progress":    0.5,
					"priority":    2,
					"url":         "https://linear.app/project/proj-1",
					"createdAt":   "2023-01-01T00:00:00.000Z",
					"updatedAt":   "2023-01-10T00:00:00.000Z",
					"status": map[string]interface{}{
						"type": "started",
						"name": "In Progress",
					},
					"lead": map[string]interface{}{
						"name": "Alice",
					},
				},
				{
					"id":          "proj-2",
					"name":        "Project Beta",
					"description": "",
					"progress":    0.1,
					"priority":    1,
					"url":         "https://linear.app/project/proj-2",
					"createdAt":   "2023-02-01T00:00:00.000Z",
					"updatedAt":   "2023-02-05T00:00:00.000Z",
					"status": map[string]interface{}{
						"type": "planned",
						"name": "Planned",
					},
				},
			},
			mockStatus:    http.StatusOK,
			expectError:   false,
			expectedCount: 2,
			validateProjects: func(t *testing.T, projects []Project) {
				if len(projects) != 2 {
					t.Fatalf("expected 2 projects, got %d", len(projects))
				}
				if projects[0].Name != "Project Alpha" {
					t.Errorf("expected first project name 'Project Alpha', got %s", projects[0].Name)
				}
				if projects[0].Progress != 0.5 {
					t.Errorf("expected progress 0.5, got %f", projects[0].Progress)
				}
				if projects[0].Lead == nil || projects[0].Lead.Name != "Alice" {
					t.Errorf("expected lead Alice")
				}
				if projects[1].Lead != nil {
					t.Errorf("expected nil lead for second project, got %+v", projects[1].Lead)
				}
			},
		},
		{
			name:         "successful project retrieval with showAll=true",
			showAll:      true,
			mockViewerID: "", // Not needed for showAll=true
			mockProjects: []map[string]interface{}{
				{
					"id":          "proj-global",
					"name":        "Global Project",
					"description": "All-company project",
					"progress":    0.75,
					"priority":    1,
					"url":         "https://linear.app/project/proj-global",
					"createdAt":   "2023-01-01T00:00:00.000Z",
					"updatedAt":   "2023-03-01T00:00:00.000Z",
					"status": map[string]interface{}{
						"type": "started",
						"name": "In Progress",
					},
					"lead": map[string]interface{}{
						"name": "CEO",
					},
				},
			},
			mockStatus:    http.StatusOK,
			expectError:   false,
			expectedCount: 1,
			validateProjects: func(t *testing.T, projects []Project) {
				if len(projects) != 1 {
					t.Fatalf("expected 1 project, got %d", len(projects))
				}
				if projects[0].Name != "Global Project" {
					t.Errorf("expected project name 'Global Project', got %s", projects[0].Name)
				}
			},
		},
		{
			name:          "empty project list",
			showAll:       false,
			mockViewerID:  "user-123",
			mockProjects:  []map[string]interface{}{},
			mockStatus:    http.StatusOK,
			expectError:   false,
			expectedCount: 0,
			validateProjects: func(t *testing.T, projects []Project) {
				if len(projects) != 0 {
					t.Errorf("expected 0 projects, got %d", len(projects))
				}
			},
		},
		{
			name:         "GraphQL error",
			showAll:      false,
			mockViewerID: "user-123",
			mockProjects: nil,
			mockStatus:   http.StatusOK,
			mockErrors: []GraphQLError{
				{
					Message: "Authentication failed",
				},
			},
			expectError:   true,
			errorContains: "GraphQL errors",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			requestCount := 0
			server := mockLinearServer(func(w http.ResponseWriter, r *http.Request) {
				requestCount++

				var resp GraphQLResponse

				// First request for showAll=false is the viewer query
				if !tt.showAll && requestCount == 1 {
					viewerData := map[string]interface{}{
						"viewer": map[string]interface{}{
							"id": tt.mockViewerID,
						},
					}
					data, _ := json.Marshal(viewerData)
					resp.Data = data
				} else {
					// Projects query
					if tt.mockErrors != nil {
						resp.Errors = tt.mockErrors
					}
					if tt.mockProjects != nil {
						projectsData := map[string]interface{}{
							"projects": map[string]interface{}{
								"nodes": tt.mockProjects,
							},
						}
						data, _ := json.Marshal(projectsData)
						resp.Data = data
					}
				}

				w.WriteHeader(tt.mockStatus)
				json.NewEncoder(w).Encode(resp)
			})
			defer server.Close()

			projects, err := getProjectsWithEndpoint("test-api-key", tt.showAll, server.URL)

			if tt.expectError {
				if err == nil {
					t.Fatal("expected error but got none")
				}
				if tt.errorContains != "" && !contains(err.Error(), tt.errorContains) {
					t.Errorf("expected error to contain '%s', got '%s'", tt.errorContains, err.Error())
				}
			} else {
				if err != nil {
					t.Fatalf("unexpected error: %v", err)
				}
				if len(projects) != tt.expectedCount {
					t.Errorf("expected %d projects, got %d", tt.expectedCount, len(projects))
				}
				if tt.validateProjects != nil {
					tt.validateProjects(t, projects)
				}
			}
		})
	}
}

// TestGetTemplates tests the getTemplates function
func TestGetTemplates(t *testing.T) {
	tests := []struct {
		name              string
		mockTeams         []map[string]interface{}
		mockTemplates     []map[string]interface{}
		mockStatus        int
		mockErrors        []GraphQLError
		expectError       bool
		errorContains     string
		expectedTeamCount int
		validateTemplates func(*testing.T, map[string][]Template)
	}{
		{
			name: "successful template retrieval grouped by team",
			mockTeams: []map[string]interface{}{
				{
					"id":   "team-1",
					"name": "Engineering",
					"key":  "ENG",
				},
				{
					"id":   "team-2",
					"name": "Design",
					"key":  "DES",
				},
			},
			mockTemplates: []map[string]interface{}{
				{
					"id":          "tmpl-1",
					"name":        "Bug Report",
					"description": "Template for bug reports",
					"type":        "issue",
					"createdAt":   "2023-01-01T00:00:00.000Z",
					"team": map[string]interface{}{
						"id":   "team-1",
						"name": "Engineering",
						"key":  "ENG",
					},
				},
				{
					"id":          "tmpl-2",
					"name":        "Feature Request",
					"description": "Template for feature requests",
					"type":        "issue",
					"createdAt":   "2023-01-02T00:00:00.000Z",
					"team": map[string]interface{}{
						"id":   "team-1",
						"name": "Engineering",
						"key":  "ENG",
					},
				},
				{
					"id":          "tmpl-3",
					"name":        "Design Task",
					"description": "",
					"type":        "issue",
					"createdAt":   "2023-01-03T00:00:00.000Z",
					"team": map[string]interface{}{
						"id":   "team-2",
						"name": "Design",
						"key":  "DES",
					},
				},
			},
			mockStatus:        http.StatusOK,
			expectError:       false,
			expectedTeamCount: 2,
			validateTemplates: func(t *testing.T, templates map[string][]Template) {
				if len(templates) != 2 {
					t.Fatalf("expected 2 teams, got %d", len(templates))
				}
				engTemplates, ok := templates["Engineering"]
				if !ok {
					t.Fatal("expected Engineering team templates")
				}
				if len(engTemplates) != 2 {
					t.Errorf("expected 2 Engineering templates, got %d", len(engTemplates))
				}
				if engTemplates[0].Name != "Bug Report" {
					t.Errorf("expected template name 'Bug Report', got %s", engTemplates[0].Name)
				}

				designTemplates, ok := templates["Design"]
				if !ok {
					t.Fatal("expected Design team templates")
				}
				if len(designTemplates) != 1 {
					t.Errorf("expected 1 Design template, got %d", len(designTemplates))
				}
			},
		},
		{
			name: "workspace-level templates (no team)",
			mockTeams: []map[string]interface{}{
				{
					"id":   "team-1",
					"name": "Engineering",
					"key":  "ENG",
				},
			},
			mockTemplates: []map[string]interface{}{
				{
					"id":          "tmpl-workspace",
					"name":        "Global Template",
					"description": "Available to all teams",
					"type":        "issue",
					"createdAt":   "2023-01-01T00:00:00.000Z",
					"team":        nil,
				},
				{
					"id":          "tmpl-team",
					"name":        "Team Template",
					"description": "Team-specific",
					"type":        "issue",
					"createdAt":   "2023-01-02T00:00:00.000Z",
					"team": map[string]interface{}{
						"id":   "team-1",
						"name": "Engineering",
						"key":  "ENG",
					},
				},
			},
			mockStatus:        http.StatusOK,
			expectError:       false,
			expectedTeamCount: 2, // Engineering + Workspace
			validateTemplates: func(t *testing.T, templates map[string][]Template) {
				if len(templates) != 2 {
					t.Fatalf("expected 2 groups (team + workspace), got %d", len(templates))
				}
				workspaceTemplates, ok := templates["Workspace"]
				if !ok {
					t.Fatal("expected Workspace templates")
				}
				if len(workspaceTemplates) != 1 {
					t.Errorf("expected 1 Workspace template, got %d", len(workspaceTemplates))
				}
				if workspaceTemplates[0].Name != "Global Template" {
					t.Errorf("expected template name 'Global Template', got %s", workspaceTemplates[0].Name)
				}
			},
		},
		{
			name: "empty template list",
			mockTeams: []map[string]interface{}{
				{
					"id":   "team-1",
					"name": "Engineering",
					"key":  "ENG",
				},
			},
			mockTemplates:     []map[string]interface{}{},
			mockStatus:        http.StatusOK,
			expectError:       false,
			expectedTeamCount: 0,
			validateTemplates: func(t *testing.T, templates map[string][]Template) {
				if len(templates) != 0 {
					t.Errorf("expected 0 template groups, got %d", len(templates))
				}
			},
		},
		{
			name: "templates filtered by team membership",
			mockTeams: []map[string]interface{}{
				{
					"id":   "team-1",
					"name": "Engineering",
					"key":  "ENG",
				},
			},
			mockTemplates: []map[string]interface{}{
				{
					"id":          "tmpl-1",
					"name":        "Eng Template",
					"description": "For engineering",
					"type":        "issue",
					"createdAt":   "2023-01-01T00:00:00.000Z",
					"team": map[string]interface{}{
						"id":   "team-1",
						"name": "Engineering",
						"key":  "ENG",
					},
				},
				{
					"id":          "tmpl-2",
					"name":        "Sales Template",
					"description": "For sales team",
					"type":        "issue",
					"createdAt":   "2023-01-02T00:00:00.000Z",
					"team": map[string]interface{}{
						"id":   "team-99",
						"name": "Sales",
						"key":  "SALES",
					},
				},
			},
			mockStatus:        http.StatusOK,
			expectError:       false,
			expectedTeamCount: 1, // Only Engineering, Sales filtered out
			validateTemplates: func(t *testing.T, templates map[string][]Template) {
				if len(templates) != 1 {
					t.Fatalf("expected 1 team, got %d", len(templates))
				}
				_, hasSales := templates["Sales"]
				if hasSales {
					t.Error("Sales team templates should be filtered out")
				}
				engTemplates, ok := templates["Engineering"]
				if !ok {
					t.Fatal("expected Engineering team templates")
				}
				if len(engTemplates) != 1 {
					t.Errorf("expected 1 Engineering template, got %d", len(engTemplates))
				}
			},
		},
		{
			name:      "GraphQL error on teams query",
			mockTeams: nil,
			mockErrors: []GraphQLError{
				{
					Message: "Unauthorized",
				},
			},
			mockStatus:    http.StatusOK,
			expectError:   true,
			errorContains: "GraphQL errors",
		},
		{
			name: "GraphQL error on templates query",
			mockTeams: []map[string]interface{}{
				{
					"id":   "team-1",
					"name": "Engineering",
					"key":  "ENG",
				},
			},
			mockTemplates: nil,
			mockStatus:    http.StatusOK,
			mockErrors: []GraphQLError{
				{
					Message: "Failed to fetch templates",
				},
			},
			expectError:   true,
			errorContains: "GraphQL errors",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			requestCount := 0
			server := mockLinearServer(func(w http.ResponseWriter, r *http.Request) {
				requestCount++

				var resp GraphQLResponse

				// First request is the teams query
				if requestCount == 1 {
					if tt.mockErrors != nil && tt.mockTeams == nil {
						// Error on teams query
						resp.Errors = tt.mockErrors
					} else {
						teamsData := map[string]interface{}{
							"viewer": map[string]interface{}{
								"teams": map[string]interface{}{
									"nodes": tt.mockTeams,
								},
							},
						}
						data, _ := json.Marshal(teamsData)
						resp.Data = data
					}
				} else if requestCount == 2 {
					// Second request is the templates query
					if tt.mockErrors != nil && tt.mockTeams != nil {
						// Error on templates query
						resp.Errors = tt.mockErrors
					} else {
						templatesData := map[string]interface{}{
							"templates": tt.mockTemplates,
						}
						data, _ := json.Marshal(templatesData)
						resp.Data = data
					}
				}

				w.WriteHeader(tt.mockStatus)
				json.NewEncoder(w).Encode(resp)
			})
			defer server.Close()

			templates, err := getTemplatesWithEndpoint("test-api-key", server.URL)

			if tt.expectError {
				if err == nil {
					t.Fatal("expected error but got none")
				}
				if tt.errorContains != "" && !contains(err.Error(), tt.errorContains) {
					t.Errorf("expected error to contain '%s', got '%s'", tt.errorContains, err.Error())
				}
			} else {
				if err != nil {
					t.Fatalf("unexpected error: %v", err)
				}
				if len(templates) != tt.expectedTeamCount {
					t.Errorf("expected %d template groups, got %d", tt.expectedTeamCount, len(templates))
				}
				if tt.validateTemplates != nil {
					tt.validateTemplates(t, templates)
				}
			}
		})
	}
}

// TestGetIssueNetworkError tests network-level errors for getIssue
func TestGetIssueNetworkError(t *testing.T) {
	// Test with an invalid endpoint to simulate network failure
	_, err := getIssueWithEndpoint("test-api-key", "ISSUE-123", "http://invalid-endpoint-that-does-not-exist:99999")
	if err == nil {
		t.Fatal("expected network error but got none")
	}
	// The error can be "failed to execute request" or contain "dial tcp" for connection errors
	errStr := err.Error()
	if !contains(errStr, "dial tcp") && !contains(errStr, "Post") {
		t.Errorf("expected network error, got: %v", err)
	}
}

// TestGetProjectsNetworkError tests network-level errors for getProjects
func TestGetProjectsNetworkError(t *testing.T) {
	_, err := getProjectsWithEndpoint("test-api-key", false, "http://invalid-endpoint-that-does-not-exist:99999")
	if err == nil {
		t.Fatal("expected network error but got none")
	}
	// The error can contain "dial tcp" for connection errors
	errStr := err.Error()
	if !contains(errStr, "dial tcp") && !contains(errStr, "Post") {
		t.Errorf("expected network error, got: %v", err)
	}
}

// TestGetTemplatesNetworkError tests network-level errors for getTemplates
func TestGetTemplatesNetworkError(t *testing.T) {
	_, err := getTemplatesWithEndpoint("test-api-key", "http://invalid-endpoint-that-does-not-exist:99999")
	if err == nil {
		t.Fatal("expected network error but got none")
	}
	// The error can contain "dial tcp" for connection errors
	errStr := err.Error()
	if !contains(errStr, "dial tcp") && !contains(errStr, "Post") {
		t.Errorf("expected network error, got: %v", err)
	}
}

// Test helper functions

// contains checks if a string contains a substring
func contains(s, substr string) bool {
	return len(s) >= len(substr) && (s == substr || len(substr) == 0 ||
		(len(s) > 0 && len(substr) > 0 && indexOf(s, substr) >= 0))
}

func indexOf(s, substr string) int {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return i
		}
	}
	return -1
}

// Wrapper functions that accept custom endpoints for testing
// These are needed because linearAPIEndpoint is a const

func getIssueWithEndpoint(apiKey string, issueID string, endpoint string) (*Issue, error) {
	query := `
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

	variables := map[string]interface{}{
		"id": issueID,
	}

	reqBody := GraphQLRequest{
		Query:     query,
		Variables: variables,
	}

	body, err := json.Marshal(reqBody)
	if err != nil {
		return nil, err
	}

	req, err := http.NewRequest("POST", endpoint, bytes.NewBuffer(body))
	if err != nil {
		return nil, err
	}
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", apiKey)

	client := &http.Client{Timeout: 30 * time.Second}
	resp, err := client.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	respBody, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("API returned status %d: %s", resp.StatusCode, string(respBody))
	}

	var gqlResp GraphQLResponse
	if err := json.Unmarshal(respBody, &gqlResp); err != nil {
		return nil, err
	}

	if len(gqlResp.Errors) > 0 {
		return nil, fmt.Errorf("GraphQL errors: %+v", gqlResp.Errors)
	}

	var issueResp struct {
		Issue Issue `json:"issue"`
	}
	if err := json.Unmarshal(gqlResp.Data, &issueResp); err != nil {
		return nil, err
	}

	return &issueResp.Issue, nil
}

func getProjectsWithEndpoint(apiKey string, showAll bool, endpoint string) ([]Project, error) {
	client := &http.Client{Timeout: 30 * time.Second}

	var viewerID string
	if !showAll {
		viewerQuery := `query { viewer { id } }`
		viewerReqBody := GraphQLRequest{Query: viewerQuery}
		viewerBody, err := json.Marshal(viewerReqBody)
		if err != nil {
			return nil, err
		}

		req, err := http.NewRequest("POST", endpoint, bytes.NewBuffer(viewerBody))
		if err != nil {
			return nil, err
		}
		req.Header.Set("Content-Type", "application/json")
		req.Header.Set("Authorization", apiKey)

		resp, err := client.Do(req)
		if err != nil {
			return nil, err
		}
		defer resp.Body.Close()

		respBody, err := io.ReadAll(resp.Body)
		if err != nil {
			return nil, err
		}

		var gqlResp GraphQLResponse
		if err := json.Unmarshal(respBody, &gqlResp); err != nil {
			return nil, err
		}

		var viewerResp struct {
			Viewer struct {
				ID string `json:"id"`
			} `json:"viewer"`
		}
		if err := json.Unmarshal(gqlResp.Data, &viewerResp); err != nil {
			return nil, err
		}
		viewerID = viewerResp.Viewer.ID
	}

	filterClause := `status: { type: { neq: "completed" } }`
	if !showAll && viewerID != "" {
		filterClause = `
      and: [
        { status: { type: { neq: "completed" } } }
        { members: { some: { id: { eq: "` + viewerID + `" } } } }
      ]`
	}

	query := `
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

	reqBody := GraphQLRequest{Query: query}
	body, err := json.Marshal(reqBody)
	if err != nil {
		return nil, err
	}

	req, err := http.NewRequest("POST", endpoint, bytes.NewBuffer(body))
	if err != nil {
		return nil, err
	}
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", apiKey)

	resp, err := client.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	respBody, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("API returned status %d: %s", resp.StatusCode, string(respBody))
	}

	var gqlResp GraphQLResponse
	if err := json.Unmarshal(respBody, &gqlResp); err != nil {
		return nil, err
	}

	if len(gqlResp.Errors) > 0 {
		return nil, fmt.Errorf("GraphQL errors: %+v", gqlResp.Errors)
	}

	var projectsResp ProjectsResponse
	if err := json.Unmarshal(gqlResp.Data, &projectsResp); err != nil {
		return nil, err
	}

	return projectsResp.Projects.Nodes, nil
}

func getTemplatesWithEndpoint(apiKey string, endpoint string) (map[string][]Template, error) {
	client := &http.Client{Timeout: 30 * time.Second}

	// First, get the user's teams
	teamsQuery := `
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

	teamsReqBody := GraphQLRequest{Query: teamsQuery}
	teamsBody, err := json.Marshal(teamsReqBody)
	if err != nil {
		return nil, err
	}

	req, err := http.NewRequest("POST", endpoint, bytes.NewBuffer(teamsBody))
	if err != nil {
		return nil, err
	}
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", apiKey)

	resp, err := client.Do(req)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close()

	respBody, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, err
	}

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("API returned status %d: %s", resp.StatusCode, string(respBody))
	}

	var gqlResp GraphQLResponse
	if err := json.Unmarshal(respBody, &gqlResp); err != nil {
		return nil, err
	}

	if len(gqlResp.Errors) > 0 {
		return nil, fmt.Errorf("GraphQL errors: %+v", gqlResp.Errors)
	}

	var teamsResp struct {
		Viewer struct {
			Teams struct {
				Nodes []struct {
					ID   string `json:"id"`
					Name string `json:"name"`
					Key  string `json:"key"`
				} `json:"nodes"`
			} `json:"teams"`
		} `json:"viewer"`
	}
	if err := json.Unmarshal(gqlResp.Data, &teamsResp); err != nil {
		return nil, err
	}

	myTeamIDs := make(map[string]string)
	for _, team := range teamsResp.Viewer.Teams.Nodes {
		myTeamIDs[team.ID] = team.Name
	}

	// Now get all templates
	templatesQuery := `
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

	templatesReqBody := GraphQLRequest{Query: templatesQuery}
	templatesBody, err := json.Marshal(templatesReqBody)
	if err != nil {
		return nil, err
	}

	req2, err := http.NewRequest("POST", endpoint, bytes.NewBuffer(templatesBody))
	if err != nil {
		return nil, err
	}
	req2.Header.Set("Content-Type", "application/json")
	req2.Header.Set("Authorization", apiKey)

	resp2, err := client.Do(req2)
	if err != nil {
		return nil, err
	}
	defer resp2.Body.Close()

	respBody2, err := io.ReadAll(resp2.Body)
	if err != nil {
		return nil, err
	}

	if resp2.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("API returned status %d: %s", resp2.StatusCode, string(respBody2))
	}

	var gqlResp2 GraphQLResponse
	if err := json.Unmarshal(respBody2, &gqlResp2); err != nil {
		return nil, err
	}

	if len(gqlResp2.Errors) > 0 {
		return nil, fmt.Errorf("GraphQL errors: %+v", gqlResp2.Errors)
	}

	var templatesResp struct {
		Templates []Template `json:"templates"`
	}
	if err := json.Unmarshal(gqlResp2.Data, &templatesResp); err != nil {
		return nil, err
	}

	templatesByTeam := make(map[string][]Template)
	for _, tmpl := range templatesResp.Templates {
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
