package main

import (
	"encoding/json"
	"fmt"
	"time"
)

// buildAddCommentMutation returns a mutation to add a comment to an issue
func buildAddCommentMutation() string {
	return `
mutation AddComment($issueId: String!, $body: String!) {
  commentCreate(
    input: {
      issueId: $issueId
      body: $body
    }
  ) {
    success
    comment {
      id
      body
      createdAt
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

// Comment represents a comment on an issue
type Comment struct {
	ID        string    `json:"id"`
	Body      string    `json:"body"`
	CreatedAt time.Time `json:"createdAt"`
	User      struct {
		Name string `json:"name"`
	} `json:"user"`
	Issue struct {
		Identifier string `json:"identifier"`
	} `json:"issue"`
	URL string `json:"url"`
}

// parseAddCommentResponse parses a comment create mutation response
func parseAddCommentResponse(data json.RawMessage) (*Comment, error) {
	var commentResp struct {
		CommentCreate struct {
			Success bool    `json:"success"`
			Comment Comment `json:"comment"`
		} `json:"commentCreate"`
	}
	if err := json.Unmarshal(data, &commentResp); err != nil {
		return nil, fmt.Errorf("failed to unmarshal data: %w", err)
	}
	if !commentResp.CommentCreate.Success {
		return nil, fmt.Errorf("comment creation failed")
	}
	return &commentResp.CommentCreate.Comment, nil
}

// addComment adds a comment to an issue
func addComment(apiKey, issueID, commentBody string) (*Comment, error) {
	variables := map[string]interface{}{
		"issueId": issueID,
		"body":    commentBody,
	}
	data, err := executeGraphQL(apiKey, buildAddCommentMutation(), variables)
	if err != nil {
		return nil, err
	}
	return parseAddCommentResponse(data)
}
