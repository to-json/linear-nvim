package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"time"
)

const linearAPIEndpoint = "https://api.linear.app/graphql"

// Linear API has a complexity limit of 10000
const maxGraphQLComplexity = 10000

// GraphQLRequest represents a GraphQL request payload
type GraphQLRequest struct {
	Query     string                 `json:"query"`
	Variables map[string]interface{} `json:"variables,omitempty"`
}

// GraphQLResponse represents a GraphQL response payload
type GraphQLResponse struct {
	Data   json.RawMessage `json:"data"`
	Errors []GraphQLError  `json:"errors,omitempty"`
}

// GraphQLError represents a GraphQL error
type GraphQLError struct {
	Message string `json:"message"`
	Path    []any  `json:"path,omitempty"`
}

// executeGraphQL performs a GraphQL request and returns the raw response data
func executeGraphQL(apiKey, query string, variables map[string]interface{}) (json.RawMessage, error) {
	return executeGraphQLWithEndpoint(apiKey, query, variables, linearAPIEndpoint)
}

// executeGraphQLWithEndpoint performs a GraphQL request to a custom endpoint
// This function is used primarily for testing but also provides flexibility
func executeGraphQLWithEndpoint(apiKey, query string, variables map[string]interface{}, endpoint string) (json.RawMessage, error) {
	// Validate query is not empty
	if query == "" {
		return nil, fmt.Errorf("query cannot be empty")
	}

	// Basic query length validation (approximate complexity check)
	// This is a simple heuristic - Linear's actual complexity calculation is more sophisticated
	if len(query) > 50000 {
		return nil, fmt.Errorf("query exceeds maximum length (possible complexity issue)")
	}

	// Build the request
	reqBody := GraphQLRequest{
		Query:     query,
		Variables: variables,
	}

	body, err := json.Marshal(reqBody)
	if err != nil {
		return nil, fmt.Errorf("failed to marshal request: %w", err)
	}

	req, err := http.NewRequest("POST", endpoint, bytes.NewBuffer(body))
	if err != nil {
		return nil, fmt.Errorf("failed to create request: %w", err)
	}

	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("Authorization", apiKey)

	client := &http.Client{Timeout: 30 * time.Second}
	resp, err := client.Do(req)
	if err != nil {
		return nil, fmt.Errorf("failed to execute request: %w", err)
	}
	defer resp.Body.Close()

	respBody, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("failed to read response: %w", err)
	}

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("API returned status %d: %s", resp.StatusCode, string(respBody))
	}

	var gqlResp GraphQLResponse
	if err := json.Unmarshal(respBody, &gqlResp); err != nil {
		return nil, fmt.Errorf("failed to unmarshal response: %w", err)
	}

	if len(gqlResp.Errors) > 0 {
		return nil, fmt.Errorf("GraphQL errors: %+v", gqlResp.Errors)
	}

	return gqlResp.Data, nil
}
