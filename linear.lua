local M = {}

-- Try to find linear-cli in GOPATH first, fall back to local path
local function find_cli()
  local gopath = vim.fn.system('go env GOPATH'):gsub('\n', '')
  local gopath_cli = gopath .. '/bin/linear-cli'

  if vim.fn.executable(gopath_cli) == 1 then
    return gopath_cli
  end

  -- Fallback to local path
  return vim.fn.expand('$HOME/hc/linear-cli/linear-cli')
end

local LINEAR_CLI = find_cli()

local function parse_json(output)
  local ok, result = pcall(vim.json.decode, output)
  if not ok then
    return nil, "Failed to parse JSON: " .. result
  end
  return result, nil
end

-- Executes linear-cli command asynchronously and returns parsed JSON result via callback.
-- Callback signature: callback(result, error_message)
local function run_cli(args, callback)
  local stdout = {}
  local stderr = {}

  vim.fn.jobstart(LINEAR_CLI .. ' ' .. args, {
    stdout_buffered = true,
    stderr_buffered = true,
    on_stdout = function(_, data)
      if data then
        vim.list_extend(stdout, data)
      end
    end,
    on_stderr = function(_, data)
      if data then
        vim.list_extend(stderr, data)
      end
    end,
    on_exit = function(_, code)
      if code ~= 0 then
        local err = table.concat(stderr, '\n')
        callback(nil, "Command failed: " .. err)
      else
        local output = table.concat(stdout, '\n')
        local parsed, parse_err = parse_json(output)
        if parse_err then
          callback(nil, parse_err)
        else
          callback(parsed, nil)
        end
      end
    end,
  })
end

local function fetch_teams(callback)
  run_cli('teams --json', callback)
end

local function fetch_projects(team_id, callback)
  run_cli('projects --json', function(projects, err)
    if err then
      callback(nil, err)
      return
    end
    callback(projects, nil)
  end)
end

local function fetch_templates(callback)
  run_cli('templates --json', callback)
end

-- Parses issue creation buffer into structured data.
-- Recognizes section headers (# TITLE:, # DESCRIPTION:, etc.) and extracts content.
-- Returns: {title, description, priority}
local function parse_buffer(bufnr)
  local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)

  local issue = {
    title = "",
    description = "",
    priority = 0,
  }

  local current_section = nil
  local content_lines = {}

  for _, line in ipairs(lines) do
    if line:match("^# TITLE:") then
      current_section = "title"
      content_lines = {}
    elseif line:match("^# DESCRIPTION:") then
      if current_section == "title" then
        issue.title = table.concat(content_lines, " "):gsub("^%s*(.-)%s*$", "%1")
      end
      current_section = "description"
      content_lines = {}
    elseif line:match("^# PRIORITY:") then
      if current_section == "description" then
        issue.description = table.concat(content_lines, "\n"):gsub("^%s*(.-)%s*$", "%1")
      end
      current_section = "priority"
      content_lines = {}
    elseif line:match("^# PROJECT:") then
      if current_section == "priority" then
        local prio_str = table.concat(content_lines, " "):gsub("^%s*(.-)%s*$", "%1")
        issue.priority = tonumber(prio_str) or 0
      end
      current_section = "project"
      content_lines = {}
    elseif line:match("^# TEAM:") then
      current_section = "team"
      content_lines = {}
    elseif not line:match("^#") then
      if current_section and not line:match("^%s*$") or #content_lines > 0 then
        table.insert(content_lines, line)
      end
    end
  end

  if current_section == "description" then
    issue.description = table.concat(content_lines, "\n"):gsub("^%s*(.-)%s*$", "%1")
  elseif current_section == "priority" then
    local prio_str = table.concat(content_lines, " "):gsub("^%s*(.-)%s*$", "%1")
    issue.priority = tonumber(prio_str) or 0
  end

  return issue
end

local function create_issue_buffer(team, project, template)
  local bufnr = vim.api.nvim_create_buf(false, true)

  vim.api.nvim_buf_set_option(bufnr, 'buftype', 'acwrite')
  vim.api.nvim_buf_set_option(bufnr, 'filetype', 'markdown')

  local lines = {
    "# LINEAR ISSUE",
    "# Team: " .. team.name .. " (" .. team.key .. ")",
    "",
    "# PROJECT:",
  }

  if project then
    table.insert(lines, project.name)
  else
    table.insert(lines, "(none)")
  end

  table.insert(lines, "")
  table.insert(lines, "# TITLE:")

  if template and template.title then
    table.insert(lines, template.title)
  else
    table.insert(lines, "")
  end

  table.insert(lines, "")
  table.insert(lines, "# DESCRIPTION:")

  if template and template.description then
    table.insert(lines, template.description)
  else
    table.insert(lines, "")
  end

  table.insert(lines, "")
  table.insert(lines, "# PRIORITY: (0=None, 1=Urgent, 2=High, 3=Normal, 4=Low)")

  if template and template.priority then
    table.insert(lines, tostring(template.priority))
  else
    table.insert(lines, "0")
  end

  table.insert(lines, "")
  table.insert(lines, "# ---")
  table.insert(lines, "# Save this buffer (:w) to create the issue")
  table.insert(lines, "# Close without saving (:q!) to cancel")

  vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, lines)

  vim.api.nvim_buf_set_var(bufnr, 'linear_team_id', team.id)
  if project then
    vim.api.nvim_buf_set_var(bufnr, 'linear_project_id', project.id)
  end

  vim.api.nvim_create_autocmd('BufWriteCmd', {
    buffer = bufnr,
    callback = function()
      M.submit_issue(bufnr, team, project)
    end,
  })

  return bufnr
end

function M.submit_issue(bufnr, team, project)
  local issue = parse_buffer(bufnr)

  if issue.title == "" then
    vim.notify("Title is required", vim.log.levels.ERROR)
    return
  end

  local cmd = string.format(
    'cd %s && ./linear-cli create --team=%s --title=%s --priority=%d',
    vim.fn.expand('$HOME/hc/linear-cli'),
    vim.fn.shellescape(team.id),
    vim.fn.shellescape(issue.title),
    issue.priority
  )

  if issue.description ~= "" then
    cmd = cmd .. ' --description=' .. vim.fn.shellescape(issue.description)
  end

  if project then
    cmd = cmd .. ' --project=' .. vim.fn.shellescape(project.id)
  end

  vim.notify("Creating issue...", vim.log.levels.INFO)

  vim.fn.jobstart(cmd, {
    stdout_buffered = true,
    on_stdout = function(_, data)
      if data then
        local output = table.concat(data, '\n')
        if output:match("created successfully") or output:match("URL:") then
          vim.notify("Issue created successfully!", vim.log.levels.INFO)
          vim.api.nvim_buf_set_option(bufnr, 'modified', false)
          vim.schedule(function()
            vim.api.nvim_buf_delete(bufnr, {force = true})
          end)
        end
      end
    end,
    on_stderr = function(_, data)
      if data and #data > 0 then
        local err = table.concat(data, '\n')
        if err ~= "" then
          vim.notify("Error: " .. err, vim.log.levels.ERROR)
        end
      end
    end,
    on_exit = function(_, code)
      if code ~= 0 then
        vim.notify("Failed to create issue (exit code: " .. code .. ")", vim.log.levels.ERROR)
      end
    end,
  })
end

-- Lists issues for a user-selected project.
-- Creates read-only buffer with issue list; press <CR> on an issue line to view it.
-- Stores issue_map in buffer variable to map line numbers to issue identifiers.
function M.list_project_issues()
  vim.notify("Fetching projects...", vim.log.levels.INFO)

  run_cli('projects --json', function(projects, err)
    if err then
      vim.notify("Error fetching projects: " .. err, vim.log.levels.ERROR)
      return
    end

    if not projects or #projects == 0 then
      vim.notify("No projects found", vim.log.levels.WARN)
      return
    end

    vim.ui.select(projects, {
      prompt = 'Select project:',
      format_item = function(item)
        return item.name .. (item.state and (" [" .. item.state .. "]") or "")
      end,
    }, function(project)
      if not project then
        return
      end

      vim.notify("Fetching issues for " .. project.name .. "...", vim.log.levels.INFO)

      run_cli('issues ' .. vim.fn.shellescape(project.id) .. ' --json', function(issues, err)
        if err then
          vim.notify("Error fetching issues: " .. err, vim.log.levels.ERROR)
          return
        end

        local bufnr = vim.api.nvim_create_buf(false, true)

        vim.api.nvim_buf_set_option(bufnr, 'buftype', 'nofile')
        vim.api.nvim_buf_set_option(bufnr, 'filetype', 'linear-issues')

        local lines = {
          "PROJECT: " .. project.name,
          "=" .. string.rep("=", #project.name + 8),
          "",
        }

        local issue_map = {}

        if not issues or #issues == 0 then
          table.insert(lines, "No issues found for this project.")
        else
          for _, issue in ipairs(issues) do
            local identifier = issue.identifier or "???"
            local title = issue.title or "(no title)"

            local state_name = "unknown"
            if issue.state and type(issue.state) == "table" then
              state_name = issue.state.name or "unknown"
            elseif type(issue.state) == "string" then
              state_name = issue.state
            end

            local assignee_name = "unassigned"
            if issue.assignee and type(issue.assignee) == "table" then
              assignee_name = issue.assignee.name or "unassigned"
            elseif issue.assignee and type(issue.assignee) == "string" then
              assignee_name = issue.assignee
            end

            local priority = issue.priority or 0

            local priority_labels = {
              [0] = "None",
              [1] = "Urgent",
              [2] = "High",
              [3] = "Normal",
              [4] = "Low",
            }
            local priority_str = priority_labels[priority] or tostring(priority)

            local line_num = #lines + 1
            issue_map[line_num] = identifier

            table.insert(lines, string.format("%-10s  %s", identifier, title))
            table.insert(lines, string.format("            State: %s | Assignee: %s | Priority: %s", state_name, assignee_name, priority_str))
            table.insert(lines, "")
          end
        end

        table.insert(lines, "")
        table.insert(lines, "Press <CR> to open issue in new buffer")

        vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, lines)

        vim.api.nvim_buf_set_var(bufnr, 'linear_issue_map', issue_map)

        vim.api.nvim_buf_set_keymap(bufnr, 'n', '<CR>', '', {
          noremap = true,
          silent = true,
          callback = function()
            local cursor = vim.api.nvim_win_get_cursor(0)
            local line_num = cursor[1]
            local ok, map = pcall(vim.api.nvim_buf_get_var, bufnr, 'linear_issue_map')
            if ok and map and map[line_num] then
              M.view_issue(map[line_num])
            end
          end,
        })

        vim.api.nvim_buf_set_option(bufnr, 'modifiable', false)

        vim.api.nvim_set_current_buf(bufnr)
      end)
    end)
  end)
end

-- Interactive issue creation: prompts for team, project, template, then opens editable buffer.
function M.create_issue()
  vim.notify("Fetching teams...", vim.log.levels.INFO)

  fetch_teams(function(teams, err)
    if err then
      vim.notify("Error fetching teams: " .. err, vim.log.levels.ERROR)
      return
    end

    if not teams or #teams == 0 then
      vim.notify("No teams found", vim.log.levels.WARN)
      return
    end

    vim.ui.select(teams, {
      prompt = 'Select team:',
      format_item = function(item)
        return item.name .. " (" .. item.key .. ")"
      end,
    }, function(team)
      if not team then
        return
      end

      vim.notify("Fetching projects...", vim.log.levels.INFO)

      fetch_projects(team.id, function(projects, err)
        if err then
          vim.notify("Error fetching projects: " .. err, vim.log.levels.ERROR)
          return
        end

        local project_options = {{id = nil, name = "(No project)"}}
        if projects then
          for _, proj in ipairs(projects) do
            table.insert(project_options, proj)
          end
        end

        vim.ui.select(project_options, {
          prompt = 'Select project (optional):',
          format_item = function(item)
            return item.name
          end,
        }, function(project)
          if not project or project.id == nil then
            project = nil
          end

          vim.notify("Fetching templates...", vim.log.levels.INFO)

          fetch_templates(function(all_templates, err)
            if err then
              vim.notify("Error fetching templates: " .. err, vim.log.levels.ERROR)
              return
            end

            local team_templates = {}
            if all_templates and type(all_templates) == "table" then
              for team_name, templates in pairs(all_templates) do
                if team_name == team.name then
                  team_templates = templates
                  break
                end
              end
            end

            local template_options = {{id = nil, name = "(No template)"}}
            for _, tmpl in ipairs(team_templates) do
              table.insert(template_options, tmpl)
            end

            vim.ui.select(template_options, {
              prompt = 'Select template (optional):',
              format_item = function(item)
                return item.name
              end,
            }, function(template)
              if not template or template.id == nil then
                template = nil
              end

              local bufnr = create_issue_buffer(team, project, template)

              vim.api.nvim_set_current_buf(bufnr)

              local title_line = 7
              vim.api.nvim_win_set_cursor(0, {title_line, 0})
            end)
          end)
        end)
      end)
    end)
  end)
end

-- Adds a comment to an issue. Tries to get issue_id from argument, buffer variable, or prompts.
-- Auto-refreshes issue buffer if currently viewing the issue.
function M.add_comment(issue_id)
  if not issue_id or issue_id == "" then
    local ok, buf_issue_id = pcall(vim.api.nvim_buf_get_var, 0, 'linear_issue_id')
    if ok and buf_issue_id and buf_issue_id ~= "" then
      issue_id = buf_issue_id
    else
      vim.ui.input({
        prompt = 'Enter issue ID (e.g., ENG-123): ',
      }, function(input)
        if input and input ~= "" then
          M.add_comment(input)
        end
      end)
      return
    end
  end

  vim.ui.input({
    prompt = 'Comment text: ',
    default = '',
  }, function(comment_text)
    if not comment_text or comment_text == "" then
      vim.notify("Comment text is required", vim.log.levels.WARN)
      return
    end

    vim.notify("Adding comment to " .. issue_id .. "...", vim.log.levels.INFO)

    local cmd = string.format(
      'comment %s %s --json',
      vim.fn.shellescape(issue_id),
      vim.fn.shellescape(comment_text)
    )

    run_cli(cmd, function(result, err)
      if err then
        vim.notify("Error adding comment: " .. err, vim.log.levels.ERROR)
        return
      end

      local comment_id = result and result.id or "unknown"
      vim.notify("Comment added successfully (ID: " .. comment_id .. ")", vim.log.levels.INFO)

      local current_buf = vim.api.nvim_get_current_buf()
      local ok, buf_issue_id = pcall(vim.api.nvim_buf_get_var, current_buf, 'linear_issue_id')
      if ok and buf_issue_id == issue_id then
        vim.schedule(function()
          M.view_issue(issue_id)
        end)
      end
    end)
  end)
end

-- Parses issue view buffer to extract editable description and comments.
-- Looks for HTML comment markers (<!-- DESCRIPTION START -->, <!-- COMMENT ID: xxx START -->).
-- Strips 2-space indentation from comment bodies.
-- Returns: {description, comments: [{id, body}]}
local function parse_editable_buffer(bufnr)
  local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
  local result = {
    description = nil,
    comments = {}
  }

  local current_section = nil
  local current_comment_id = nil
  local content_lines = {}

  for _, line in ipairs(lines) do
    if line:match("^<!%-%- DESCRIPTION START %-%->") then
      current_section = "description"
      content_lines = {}
    elseif line:match("^<!%-%- DESCRIPTION END %-%->") then
      if current_section == "description" then
        result.description = table.concat(content_lines, "\n")
      end
      current_section = nil
      content_lines = {}
    elseif line:match("^<!%-%- COMMENT ID: (.+) START %-%->") then
      local comment_id = line:match("^<!%-%- COMMENT ID: (.+) START %-%->")
      current_section = "comment"
      current_comment_id = comment_id
      content_lines = {}
    elseif line:match("^<!%-%- COMMENT ID: .+ END %-%->") then
      if current_section == "comment" and current_comment_id then
        table.insert(result.comments, {
          id = current_comment_id,
          body = table.concat(content_lines, "\n")
        })
      end
      current_section = nil
      current_comment_id = nil
      content_lines = {}
    elseif current_section then
      if current_section == "comment" then
        local stripped = line:gsub("^  ", "", 1)
        table.insert(content_lines, stripped)
      else
        table.insert(content_lines, line)
      end
    end
  end

  return result
end

-- Compares buffer content against original issue data and posts updates for changed sections.
-- Normalizes whitespace before comparison to avoid spurious diffs.
-- Updates are posted sequentially; buffer auto-refreshes after all complete.
local function save_issue_changes(bufnr)
  local issue_id = vim.api.nvim_buf_get_var(bufnr, 'linear_issue_id')
  local original_issue = vim.api.nvim_buf_get_var(bufnr, 'linear_issue_data')

  local parsed = parse_editable_buffer(bufnr)

  local updates = {}

  if parsed.description then
    local parsed_desc_norm = parsed.description:gsub("^%s+", ""):gsub("%s+$", "")
    local original_desc_norm = (original_issue.description or ""):gsub("^%s+", ""):gsub("%s+$", "")

    if parsed_desc_norm ~= original_desc_norm then
      table.insert(updates, {
        type = "description",
        text = parsed.description
      })
    end
  end

  for _, parsed_comment in ipairs(parsed.comments) do
    local original_body = nil
    if original_issue.comments and original_issue.comments.nodes then
      for _, orig_comment in ipairs(original_issue.comments.nodes) do
        if orig_comment.id == parsed_comment.id then
          original_body = orig_comment.body
          break
        end
      end
    end

    local parsed_normalized = parsed_comment.body:gsub("^%s+", ""):gsub("%s+$", "")
    local original_normalized = original_body and original_body:gsub("^%s+", ""):gsub("%s+$", "") or ""

    if original_body and parsed_normalized ~= original_normalized then
      table.insert(updates, {
        type = "comment",
        id = parsed_comment.id,
        text = parsed_comment.body
      })
    end
  end

  if #updates == 0 then
    vim.notify("No changes to save", vim.log.levels.INFO)
    vim.api.nvim_buf_set_option(bufnr, 'modified', false)
    return
  end

  local update_idx = 0
  local function do_next_update()
    update_idx = update_idx + 1
    if update_idx > #updates then
      vim.notify("All changes saved successfully", vim.log.levels.INFO)
      vim.schedule(function()
        M.view_issue(issue_id)
      end)
      return
    end

    local update = updates[update_idx]
    if update.type == "description" then
      vim.notify("Updating description...", vim.log.levels.INFO)
      local cmd = string.format(
        'update-description %s %s --json',
        vim.fn.shellescape(issue_id),
        vim.fn.shellescape(update.text)
      )

      run_cli(cmd, function(result, err)
        if err then
          vim.notify("Error updating description: " .. err, vim.log.levels.ERROR)
        else
          vim.notify("Description updated", vim.log.levels.INFO)
        end
        do_next_update()
      end)
    elseif update.type == "comment" then
      vim.notify("Updating comment...", vim.log.levels.INFO)
      local cmd = string.format(
        'update-comment %s %s --json',
        vim.fn.shellescape(update.id),
        vim.fn.shellescape(update.text)
      )

      run_cli(cmd, function(result, err)
        if err then
          vim.notify("Error updating comment: " .. err, vim.log.levels.ERROR)
        else
          vim.notify("Comment updated", vim.log.levels.INFO)
        end
        do_next_update()
      end)
    end
  end

  do_next_update()
end

-- Views issue with full details and comments. Creates editable buffer.
-- Description is always editable. Comments by current user (matched by name) are editable.
-- Edit markers (<!-- ... -->) indicate editable sections. Save with :w to post changes.
-- Reuses existing buffer if one with this issue_id already exists.
function M.view_issue(issue_id)
  if not issue_id or issue_id == "" then
    vim.ui.input({
      prompt = 'Enter issue ID (e.g., ENG-123): ',
    }, function(input)
      if input and input ~= "" then
        M.view_issue(input)
      end
    end)
    return
  end

  vim.notify("Fetching issue " .. issue_id .. "...", vim.log.levels.INFO)

  run_cli('get issue ' .. vim.fn.shellescape(issue_id) .. ' --json', function(issue, err)
    if err then
      vim.notify("Error fetching issue: " .. err, vim.log.levels.ERROR)
      return
    end

    if not issue then
      vim.notify("No issue data returned", vim.log.levels.ERROR)
      return
    end

    local current_user_name = nil
    if issue.assignee and type(issue.assignee) == "table" and issue.assignee.name then
      current_user_name = issue.assignee.name
    elseif issue.creator and type(issue.creator) == "table" and issue.creator.name then
      current_user_name = issue.creator.name
    end

    local buffer_name = 'linear://' .. issue_id
    local existing_bufnr = nil
    for _, buf in ipairs(vim.api.nvim_list_bufs()) do
      if vim.api.nvim_buf_is_valid(buf) then
        local name = vim.api.nvim_buf_get_name(buf)
        if name == buffer_name then
          existing_bufnr = buf
          break
        end
      end
    end

    local bufnr
    if existing_bufnr then
      bufnr = existing_bufnr
      vim.api.nvim_buf_set_option(bufnr, 'modifiable', true)
    else
      bufnr = vim.api.nvim_create_buf(false, true)
      vim.api.nvim_buf_set_name(bufnr, buffer_name)
    end

    vim.api.nvim_buf_set_option(bufnr, 'buftype', 'acwrite')
    vim.api.nvim_buf_set_option(bufnr, 'filetype', 'markdown')

    vim.api.nvim_buf_set_var(bufnr, 'linear_issue_id', issue_id)
    vim.api.nvim_buf_set_var(bufnr, 'linear_issue_data', issue)

    local lines = {}

    local identifier = issue.identifier or issue_id
    local title = issue.title or "(no title)"
    table.insert(lines, "# " .. identifier .. ": " .. title)
    table.insert(lines, "")

    if issue.url then
      table.insert(lines, "**URL:** " .. issue.url)
      table.insert(lines, "")
    end

    local team_name = "Unknown"
    if issue.team and type(issue.team) == "table" then
      team_name = issue.team.name or "Unknown"
    elseif type(issue.team) == "string" then
      team_name = issue.team
    end

    local state_name = "Unknown"
    if issue.state and type(issue.state) == "table" then
      state_name = issue.state.name or "Unknown"
    elseif type(issue.state) == "string" then
      state_name = issue.state
    end

    local priority = issue.priority or 0

    local priority_labels = {
      [0] = "None",
      [1] = "Urgent",
      [2] = "High",
      [3] = "Normal",
      [4] = "Low",
    }
    local priority_str = priority_labels[priority] or tostring(priority)

    table.insert(lines, "**Team:** " .. team_name)
    table.insert(lines, "**State:** " .. state_name)
    table.insert(lines, "**Priority:** " .. priority_str)

    local assignee_name = "Unassigned"
    if issue.assignee and type(issue.assignee) == "table" then
      assignee_name = issue.assignee.name or "Unknown"
    elseif issue.assignee and type(issue.assignee) == "string" then
      assignee_name = issue.assignee
    end
    table.insert(lines, "**Assignee:** " .. assignee_name)

    table.insert(lines, "")

    if issue.createdAt then
      table.insert(lines, "**Created:** " .. issue.createdAt)
    end
    if issue.updatedAt then
      table.insert(lines, "**Updated:** " .. issue.updatedAt)
    end
    if issue.completedAt then
      table.insert(lines, "**Completed:** " .. issue.completedAt)
    end

    table.insert(lines, "")
    table.insert(lines, string.rep("-", 80))
    table.insert(lines, "")

    table.insert(lines, "## Description")
    table.insert(lines, "")
    table.insert(lines, "<!-- DESCRIPTION START -->")
    if issue.description and issue.description ~= "" then
      for line in issue.description:gmatch("[^\n]*") do
        table.insert(lines, line)
      end
    else
      table.insert(lines, "(no description)")
    end
    table.insert(lines, "<!-- DESCRIPTION END -->")

    table.insert(lines, "")
    table.insert(lines, string.rep("-", 80))
    table.insert(lines, "")

    table.insert(lines, "## Comments")
    table.insert(lines, "")

    local comments_list = nil
    if issue.comments then
      if type(issue.comments) == "table" and issue.comments.nodes then
        comments_list = issue.comments.nodes
      elseif type(issue.comments) == "table" and #issue.comments > 0 then
        comments_list = issue.comments
      end
    end

    if comments_list and #comments_list > 0 then
      for i, comment in ipairs(comments_list) do
        local author_name = "Unknown"
        if comment.user and type(comment.user) == "table" then
          author_name = comment.user.name or "Unknown"
        elseif type(comment.user) == "string" then
          author_name = comment.user
        end

        local timestamp = comment.createdAt or "Unknown time"

        local is_user_comment = (current_user_name and author_name == current_user_name)

        table.insert(lines, "### Comment " .. i)
        table.insert(lines, "**" .. author_name .. "** - " .. timestamp)
        table.insert(lines, "")

        if is_user_comment and comment.id then
          table.insert(lines, "<!-- COMMENT ID: " .. comment.id .. " START -->")
        end

        if comment.body and comment.body ~= "" then
          for line in comment.body:gmatch("[^\n]*") do
            table.insert(lines, "  " .. line)
          end
        else
          table.insert(lines, "  (empty comment)")
        end

        if is_user_comment and comment.id then
          table.insert(lines, "<!-- COMMENT ID: " .. comment.id .. " END -->")
        end

        table.insert(lines, "")
      end
    else
      table.insert(lines, "(no comments)")
    end

    vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, lines)

    vim.api.nvim_create_autocmd('BufWriteCmd', {
      buffer = bufnr,
      callback = function()
        save_issue_changes(bufnr)
      end,
    })

    vim.api.nvim_buf_set_option(bufnr, 'modifiable', true)

    vim.api.nvim_set_current_buf(bufnr)

    vim.notify("Issue " .. issue_id .. " loaded (editable)", vim.log.levels.INFO)
  end)
end

-- Changes issue state. Fetches issue to get team, fetches workflow states for team, prompts for selection.
-- Auto-refreshes issue buffer if currently viewing the issue.
function M.change_state(issue_id)
  if not issue_id or issue_id == "" then
    local ok, buf_issue_id = pcall(vim.api.nvim_buf_get_var, 0, 'linear_issue_id')
    if ok and buf_issue_id and buf_issue_id ~= "" then
      issue_id = buf_issue_id
    else
      vim.ui.input({
        prompt = 'Enter issue ID (e.g., ENG-123): ',
      }, function(input)
        if input and input ~= "" then
          M.change_state(input)
        end
      end)
      return
    end
  end

  vim.notify("Fetching issue " .. issue_id .. "...", vim.log.levels.INFO)

  run_cli('get issue ' .. vim.fn.shellescape(issue_id) .. ' --json', function(issue, err)
    if err then
      vim.notify("Error fetching issue: " .. err, vim.log.levels.ERROR)
      return
    end

    if not issue or not issue.team or not issue.team.id then
      vim.notify("Unable to determine team for issue", vim.log.levels.ERROR)
      return
    end

    local team_id = issue.team.id
    vim.notify("Fetching workflow states for team " .. issue.team.name .. "...", vim.log.levels.INFO)

    run_cli('states ' .. vim.fn.shellescape(team_id) .. ' --json', function(states, err)
      if err then
        vim.notify("Error fetching workflow states: " .. err, vim.log.levels.ERROR)
        return
      end

      if not states or #states == 0 then
        vim.notify("No workflow states found for team", vim.log.levels.WARN)
        return
      end

      vim.ui.select(states, {
        prompt = 'Select new state:',
        format_item = function(item)
          return item.name .. " (" .. item.type .. ")"
        end,
      }, function(selected_state)
        if not selected_state then
          return
        end

        vim.notify("Updating issue " .. issue_id .. " to state " .. selected_state.name .. "...", vim.log.levels.INFO)

        local cmd = string.format(
          'update-state %s %s --json',
          vim.fn.shellescape(issue_id),
          vim.fn.shellescape(selected_state.id)
        )

        run_cli(cmd, function(updated_issue, err)
          if err then
            vim.notify("Error updating issue state: " .. err, vim.log.levels.ERROR)
            return
          end

          vim.notify("Issue state updated successfully to: " .. selected_state.name, vim.log.levels.INFO)

          local current_buf = vim.api.nvim_get_current_buf()
          local ok, buf_issue_id = pcall(vim.api.nvim_buf_get_var, current_buf, 'linear_issue_id')
          if ok and buf_issue_id == issue_id then
            vim.schedule(function()
              M.view_issue(issue_id)
            end)
          end
        end)
      end)
    end)
  end)
end

-- Assigns issue to a team member. Fetches issue to get team, fetches team members, prompts for selection.
-- Auto-refreshes issue buffer if currently viewing the issue.
function M.assign_issue(issue_id)
  if not issue_id or issue_id == "" then
    local ok, buf_issue_id = pcall(vim.api.nvim_buf_get_var, 0, 'linear_issue_id')
    if ok and buf_issue_id and buf_issue_id ~= "" then
      issue_id = buf_issue_id
    else
      vim.ui.input({
        prompt = 'Enter issue ID (e.g., ENG-123): ',
      }, function(input)
        if input and input ~= "" then
          M.assign_issue(input)
        end
      end)
      return
    end
  end

  vim.notify("Fetching issue " .. issue_id .. "...", vim.log.levels.INFO)

  run_cli('get issue ' .. vim.fn.shellescape(issue_id) .. ' --json', function(issue, err)
    if err then
      vim.notify("Error fetching issue: " .. err, vim.log.levels.ERROR)
      return
    end

    if not issue or not issue.team or not issue.team.id then
      vim.notify("Unable to determine team for issue", vim.log.levels.ERROR)
      return
    end

    local team_id = issue.team.id
    vim.notify("Fetching team members for " .. issue.team.name .. "...", vim.log.levels.INFO)

    run_cli('team-members ' .. vim.fn.shellescape(team_id) .. ' --json', function(members, err)
      if err then
        vim.notify("Error fetching team members: " .. err, vim.log.levels.ERROR)
        return
      end

      if not members or #members == 0 then
        vim.notify("No team members found", vim.log.levels.WARN)
        return
      end

      vim.ui.select(members, {
        prompt = 'Select assignee:',
        format_item = function(item)
          return item.name .. " (" .. item.email .. ")"
        end,
      }, function(selected_member)
        if not selected_member then
          return
        end

        vim.notify("Assigning issue " .. issue_id .. " to " .. selected_member.name .. "...", vim.log.levels.INFO)

        local cmd = string.format(
          'assign %s %s --json',
          vim.fn.shellescape(issue_id),
          vim.fn.shellescape(selected_member.id)
        )

        run_cli(cmd, function(assigned_issue, err)
          if err then
            vim.notify("Error assigning issue: " .. err, vim.log.levels.ERROR)
            return
          end

          vim.notify("Issue assigned successfully to: " .. selected_member.name, vim.log.levels.INFO)

          local current_buf = vim.api.nvim_get_current_buf()
          local ok, buf_issue_id = pcall(vim.api.nvim_buf_get_var, current_buf, 'linear_issue_id')
          if ok and buf_issue_id == issue_id then
            vim.schedule(function()
              M.view_issue(issue_id)
            end)
          end
        end)
      end)
    end)
  end)
end

function M.unassign_issue(issue_id)
  if not issue_id or issue_id == "" then
    local ok, buf_issue_id = pcall(vim.api.nvim_buf_get_var, 0, 'linear_issue_id')
    if ok and buf_issue_id and buf_issue_id ~= "" then
      issue_id = buf_issue_id
    else
      vim.ui.input({
        prompt = 'Enter issue ID (e.g., ENG-123): ',
      }, function(input)
        if input and input ~= "" then
          M.unassign_issue(input)
        end
      end)
      return
    end
  end

  vim.notify("Unassigning issue " .. issue_id .. "...", vim.log.levels.INFO)

  local cmd = string.format(
    'unassign %s --json',
    vim.fn.shellescape(issue_id)
  )

  run_cli(cmd, function(unassigned_issue, err)
    if err then
      vim.notify("Error unassigning issue: " .. err, vim.log.levels.ERROR)
      return
    end

    vim.notify("Issue unassigned successfully", vim.log.levels.INFO)

    local current_buf = vim.api.nvim_get_current_buf()
    local ok, buf_issue_id = pcall(vim.api.nvim_buf_get_var, current_buf, 'linear_issue_id')
    if ok and buf_issue_id == issue_id then
      vim.schedule(function()
        M.view_issue(issue_id)
      end)
    end
  end)
end

function M.setup()
  vim.api.nvim_create_user_command('LinearCreate', M.create_issue, {})
  vim.api.nvim_create_user_command('LinearIssue', M.create_issue, {})
  vim.api.nvim_create_user_command('LinearProjectIssues', M.list_project_issues, {})
  vim.api.nvim_create_user_command('LinearViewIssue', function(opts)
    M.view_issue(opts.args)
  end, {
    nargs = '?',
    desc = 'View a Linear issue with full details and comments',
  })
  vim.api.nvim_create_user_command('LinearAddComment', function(opts)
    M.add_comment(opts.args)
  end, {
    nargs = '?',
    desc = 'Add a comment to a Linear issue',
  })
  vim.api.nvim_create_user_command('LinearChangeState', function(opts)
    M.change_state(opts.args)
  end, {
    nargs = '?',
    desc = 'Change the state of a Linear issue',
  })
  vim.api.nvim_create_user_command('LinearAssign', function(opts)
    M.assign_issue(opts.args)
  end, {
    nargs = '?',
    desc = 'Assign a Linear issue to a team member',
  })
  vim.api.nvim_create_user_command('LinearUnassign', function(opts)
    M.unassign_issue(opts.args)
  end, {
    nargs = '?',
    desc = 'Unassign a Linear issue (remove assignee)',
  })
end

return M
