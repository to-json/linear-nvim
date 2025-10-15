# Linear GraphQL API Guide

Quick reference for common Linear operations via GraphQL.

## 1. Reading Tickets/Issues

### Query: `issue`
Get a single issue by ID.

```graphql
query GetIssue {
  issue(id: "issue-id") {
    id
    identifier
    title
    description
    state {
      id
      name
      type
    }
    assignee {
      id
      name
      email
    }
    creator {
      id
      name
    }
    team {
      id
      name
      key
    }
    priority
    priorityLabel
    estimate
    dueDate
    createdAt
    updatedAt
    completedAt
    url
    labels {
      nodes {
        id
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
```

### Query: `issues`
List all issues with filtering and pagination.

**Signature:**
```graphql
issues(
  after: String
  before: String
  first: Int            # default 50
  last: Int
  filter: IssueFilter
  includeArchived: Boolean
  orderBy: PaginationOrderBy
  sort: [IssueSortInput!]
): IssueConnection!
```

**Key Filter Fields:**
- `assignee: NullableUserFilter` - Filter by assignee
- `createdAt: DateComparator` - Filter by creation date
- `state: WorkflowStateFilter` - Filter by workflow state
- `team: TeamFilter` - Filter by team
- `priority: NullablePriorityComparator` - Filter by priority (0=None, 1=Urgent, 2=High, 3=Normal, 4=Low)
- `labelIds: [String!]` - Filter by label IDs
- `and: [IssueFilter!]` - Combine multiple filters
- `or: [IssueFilter!]` - Match any of multiple filters

**Example:**
```graphql
query ListIssues {
  issues(
    first: 25
    filter: {
      assignee: { id: { eq: "user-id" } }
      state: { type: { eq: "started" } }
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
      createdAt
      url
    }
    pageInfo {
      hasNextPage
      endCursor
    }
  }
}
```

### Query: `viewer`
Get current user with their assigned issues.

```graphql
query ViewerIssues {
  viewer {
    id
    name
    email
    assignedIssues(
      first: 50
      filter: {
        state: { type: { neq: "completed" } }
      }
    ) {
      nodes {
        id
        identifier
        title
        priority
        state {
          name
        }
        dueDate
        url
      }
    }
  }
}
```

### Accessing User Issues
Users have multiple issue collections:
- `assignedIssues(filter: IssueFilter, ...)` - Issues assigned to user
- `createdIssues(filter: IssueFilter, ...)` - Issues created by user

---

## 2. Creating Tickets

### Mutation: `issueCreate`
Create a new issue.

**Signature:**
```graphql
issueCreate(input: IssueCreateInput!): IssuePayload!
```

**Key Input Fields:**
- `teamId: String!` - REQUIRED: Team identifier
- `title: String` - Issue title
- `description: String` - Issue description (markdown)
- `assigneeId: String` - User to assign
- `stateId: String` - Workflow state ID
- `priority: Int` - Priority (0=None, 1=Urgent, 2=High, 3=Normal, 4=Low)
- `estimate: Int` - Complexity estimate
- `dueDate: TimelessDate` - Due date
- `labelIds: [String!]` - Label IDs
- `parentId: String` - Parent issue ID (for sub-issues)
- `projectId: String` - Project ID
- `cycleId: String` - Cycle ID
- `templateId: String` - Template ID to use
- `subscriberIds: [String!]` - User IDs to subscribe

**Example:**
```graphql
mutation CreateIssue {
  issueCreate(
    input: {
      teamId: "team-id"
      title: "Fix login bug"
      description: "Users cannot log in with SSO"
      assigneeId: "user-id"
      priority: 1
      labelIds: ["label-id-1", "label-id-2"]
      estimate: 3
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
```

---

## 3. Reading Templates

### Query: `template`
Get a single template by ID.

```graphql
query GetTemplate {
  template(id: "template-id") {
    id
    name
    description
    type
    templateData
    team {
      id
      name
    }
    creator {
      name
    }
    createdAt
    updatedAt
  }
}
```

### Query: `templates`
List all templates (simple array, no pagination).

```graphql
query ListTemplates {
  templates {
    id
    name
    description
    type
    team {
      id
      name
    }
    createdAt
  }
}
```

### Query: Team-specific templates
Get templates for a specific team via the Team type.

```graphql
query TeamTemplates {
  team(id: "team-id") {
    templates(
      first: 50
      filter: { type: { eq: "issue" } }
    ) {
      nodes {
        id
        name
        description
        templateData
      }
    }
  }
}
```

**Template Fields:**
- `id: ID!` - Template identifier
- `name: String!` - Template name
- `description: String` - Template description
- `type: String!` - Entity type (e.g., "issue")
- `templateData: JSON!` - Template configuration data
- `team: Team` - Associated team (null = workspace-global)
- `organization: Organization!` - Organization
- `creator: User` - User who created template
- `inheritedFrom: Template` - Original template if inherited

---

## 4. Commenting on Tickets

### Mutation: `commentCreate`
Add a comment to an issue.

**Signature:**
```graphql
commentCreate(input: CommentCreateInput!): CommentPayload!
```

**Key Input Fields:**
- `issueId: String` - Issue to comment on
- `body: String` - Comment content (markdown)
- `parentId: String` - Parent comment ID (for threaded replies)
- `createAsUser: String` - Create as different user (OAuth apps only)
- `doNotSubscribeToIssue: Boolean` - Prevent auto-subscription

**Example:**
```graphql
mutation AddComment {
  commentCreate(
    input: {
      issueId: "issue-id"
      body: "This has been fixed in PR #123"
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
```

**Comment Fields Available:**
- `id: ID!` - Comment identifier
- `body: String!` - Comment content (markdown)
- `issue: Issue` - Associated issue
- `user: User` - User who wrote comment
- `parent: Comment` - Parent comment (if threaded)
- `children: CommentConnection` - Reply comments
- `createdAt: DateTime!` - Creation timestamp
- `editedAt: DateTime` - Last edit timestamp
- `resolvedAt: DateTime` - Resolution timestamp
- `resolvingUser: User` - User who resolved thread
- `url: String!` - Comment URL

---

## 5. Searching for Tickets

### Query: `issueSearch`
Search issues with text query and filters.

**Signature:**
```graphql
issueSearch(
  after: String
  before: String
  first: Int
  last: Int
  filter: IssueFilter
  query: String          # [DEPRECATED] Search string
  includeArchived: Boolean
  orderBy: PaginationOrderBy
): IssueConnection!
```

**Note:** The `query` parameter is deprecated. Use `filter` for all searching.

**Example - Search by text in filter:**
```graphql
query SearchIssues {
  issueSearch(
    first: 20
    filter: {
      title: { contains: "login" }
      team: { key: { eq: "ENG" } }
    }
  ) {
    nodes {
      id
      identifier
      title
      state {
        name
      }
      assignee {
        name
      }
    }
  }
}
```

### Using `issues` query for filtering
The standard `issues` query supports the same `IssueFilter` and is the primary way to filter issues.

**Example - Complex filtering:**
```graphql
query FilterIssues {
  issues(
    first: 50
    filter: {
      and: [
        { state: { type: { in: ["started", "unstarted"] } } }
        { priority: { gte: 2 } }
        { or: [
          { assignee: { id: { eq: "user-1" } } }
          { assignee: { id: { eq: "user-2" } } }
        ]}
      ]
    }
    orderBy: updatedAt
  ) {
    nodes {
      identifier
      title
      priority
      priorityLabel
      state {
        name
      }
      assignee {
        name
      }
      updatedAt
    }
  }
}
```

**Common Filter Patterns:**
```graphql
# By identifier (e.g., "ENG-123")
filter: { identifier: { eq: "ENG-123" } }

# By team key
filter: { team: { key: { eq: "ENG" } } }

# By state type
filter: { state: { type: { eq: "started" } } }

# By date range
filter: {
  createdAt: {
    gte: "2024-01-01T00:00:00Z"
    lte: "2024-12-31T23:59:59Z"
  }
}

# By multiple labels (AND)
filter: {
  labels: {
    every: { id: { in: ["label-1", "label-2"] } }
  }
}

# Unassigned issues
filter: { assignee: { null: true } }

# Exclude completed
filter: { state: { type: { neq: "completed" } } }
```

---

## Pagination

All paginated queries use cursor-based pagination:

```graphql
{
  issues(first: 25, after: "cursor-string") {
    nodes {
      # issue fields
    }
    pageInfo {
      hasNextPage
      hasPreviousPage
      startCursor
      endCursor
    }
  }
}
```

- `first: Int` - Forward pagination (get next N items)
- `after: String` - Cursor to start from
- `last: Int` - Backward pagination (get previous N items)
- `before: String` - Cursor to end at

---

## Common Types

### IssueConnection
```graphql
{
  nodes: [Issue!]!
  edges: [IssueEdge!]!
  pageInfo: PageInfo!
}
```

### WorkflowState
```graphql
{
  id: ID!
  name: String!
  type: String!        # "triage", "backlog", "unstarted", "started", "completed", "canceled"
  color: String!
  position: Float!
}
```

### Team
```graphql
{
  id: ID!
  name: String!
  key: String!         # e.g., "ENG"
  description: String
}
```

### User
```graphql
{
  id: ID!
  name: String!
  email: String!
  displayName: String!
  active: Boolean!
  admin: Boolean!
}
```

---

## Authentication

Include your API key in the Authorization header:

```
Authorization: Bearer YOUR_API_KEY
```

API endpoint:
```
https://api.linear.app/graphql
```
