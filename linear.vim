vim9script

# Linear CLI plugin for Vim9
# Complete port of the Neovim Lua plugin with all features

# Capture script ID for autocmds
const SID = expand('<SID>')

# Configuration with defaults
var config = {
  export_dir: expand('$HOME') .. '/.linear/past-tickets'
}

# Try to find linear-cli in GOPATH first, fall back to local path
def FindCli(): string
  var gopath = system('go env GOPATH')->trim()
  var gopath_cli = gopath .. '/bin/linear-cli'

  if executable(gopath_cli)
    return gopath_cli
  endif

  # Fallback to local path
  return expand('$HOME/hc/linear-cli/linear-cli')
enddef

var LINEAR_CLI = FindCli()

# Executes linear-cli command synchronously and returns parsed JSON result.
# Returns [result, error_message] tuple
def RunCli(args: string): list<any>
  var cmd = LINEAR_CLI .. ' ' .. args
  var output = system(cmd)
  var exit_code = v:shell_error

  if exit_code != 0
    return [null_dict, 'Command failed: ' .. output]
  endif

  try
    var result = json_decode(output)
    return [result, null_string]
  catch
    return [null_dict, 'Failed to parse JSON: ' .. v:exception]
  endtry
enddef

# Resolves issue_id from multiple sources.
# Detection order:
#   1. Explicit issue_id argument (if non-empty string)
#   2. Buffer variable 'linear_issue_id' (when viewing an issue buffer) [optional, can skip]
#   3. Buffer variable 'linear_issue_map' + cursor position (when in issue list buffer)
#   4. Prompt user via input() (fallback)
# skip_buffer: if true, skip checking linear_issue_id from current buffer (step 2)
def ResolveIssueId(issue_id: string, skip_buffer: bool = false): string
  # Check explicit argument
  if !empty(issue_id)
    return issue_id
  endif

  # Try to get from current buffer's linear_issue_id (unless skip_buffer is true)
  if !skip_buffer
    try
      var buf_issue_id = getbufvar(bufnr('%'), 'linear_issue_id')
      if !empty(buf_issue_id)
        return buf_issue_id
      endif
    catch
      # Continue to next method
    endtry
  endif

  # Try to get from issue list buffer's linear_issue_map
  try
    var issue_map = getbufvar(bufnr('%'), 'linear_issue_map')
    if !empty(issue_map)
      var line_num = line('.')
      if has_key(issue_map, line_num) && !empty(issue_map[line_num])
        return issue_map[line_num]
      endif
    endif
  catch
    # Continue to fallback
  endtry

  # Fallback: prompt user for issue ID
  var input_id = input('Enter issue ID (e.g., ENG-123): ')
  return input_id
enddef

# Shows a numbered menu and returns the selected item
def ShowMenu(items: list<any>, prompt: string, Format_func: func(any): string): any
  if empty(items)
    return null_dict
  endif

  echo prompt
  var idx = 1
  for item in items
    echo idx .. '. ' .. Format_func(item)
    idx += 1
  endfor

  var choice = input('Select (1-' .. len(items) .. ', 0 to cancel): ')
  var choice_num = str2nr(choice)

  if choice_num < 1 || choice_num > len(items)
    return null_dict
  endif

  return items[choice_num - 1]
enddef

# Parses issue creation buffer into structured data.
def ParseBuffer(bufnr: number): dict<any>
  var lines = getbufline(bufnr, 1, '$')

  var issue = {
    title: '',
    description: '',
    priority: 0,
  }

  var current_section = null_string
  var content_lines = []

  for line in lines
    if line =~# '^# TITLE:'
      current_section = 'title'
      content_lines = []
    elseif line =~# '^# DESCRIPTION:'
      if current_section == 'title'
        issue.title = join(content_lines, ' ')->trim()
      endif
      current_section = 'description'
      content_lines = []
    elseif line =~# '^# PRIORITY:'
      if current_section == 'description'
        issue.description = join(content_lines, "\n")->trim()
      endif
      current_section = 'priority'
      content_lines = []
    elseif line =~# '^# PROJECT:'
      if current_section == 'priority'
        var prio_str = join(content_lines, ' ')->trim()
        issue.priority = str2nr(prio_str)
      endif
      current_section = 'project'
      content_lines = []
    elseif line =~# '^# TEAM:'
      current_section = 'team'
      content_lines = []
    elseif line !~# '^#'
      if !empty(current_section) && (!empty(trim(line)) || !empty(content_lines))
        add(content_lines, line)
      endif
    endif
  endfor

  if current_section == 'description'
    issue.description = join(content_lines, "\n")->trim()
  elseif current_section == 'priority'
    var prio_str = join(content_lines, ' ')->trim()
    issue.priority = str2nr(prio_str)
  endif

  return issue
enddef

# Creates issue buffer for editing
def CreateIssueBuffer(team: dict<any>, project: any, template: any): number
  var project_part = ''
  if type(project) == v:t_dict && has_key(project, 'name')
    project_part = '/' .. substitute(project.name, '[^[:alnum:]-]', '_', 'g')
  endif

  var bufnr = bufadd('linear-create://' .. team.key .. project_part)
  bufload(bufnr)

  setbufvar(bufnr, '&buftype', 'acwrite')
  setbufvar(bufnr, '&filetype', 'markdown')

  var lines = [
    '# LINEAR ISSUE',
    '# Team: ' .. team.name .. ' (' .. team.key .. ')',
    '',
    '# PROJECT:',
  ]

  if type(project) == v:t_dict && has_key(project, 'name')
    add(lines, project.name)
  else
    add(lines, '(none)')
  endif

  extend(lines, ['', '# TITLE:'])

  if type(template) == v:t_dict && has_key(template, 'title')
    add(lines, template.title)
  else
    add(lines, '')
  endif

  extend(lines, ['', '# DESCRIPTION:'])

  if type(template) == v:t_dict && has_key(template, 'description')
    add(lines, template.description)
  else
    add(lines, '')
  endif

  extend(lines, [
    '',
    '# PRIORITY: (0=None, 1=Urgent, 2=High, 3=Normal, 4=Low)',
  ])

  if type(template) == v:t_dict && has_key(template, 'priority')
    add(lines, string(template.priority))
  else
    add(lines, '0')
  endif

  extend(lines, [
    '',
    '# ---',
    '# Save this buffer (:w) to create the issue',
    '# Close without saving (:q!) to cancel',
  ])

  setbufline(bufnr, 1, lines)

  setbufvar(bufnr, 'linear_team', team)
  setbufvar(bufnr, 'linear_project', project)

  augroup LinearCreateIssue
    autocmd! * <buffer>
    execute 'autocmd BufWriteCmd <buffer=' .. bufnr .. '> call ' .. SID .. 'SubmitIssue(' .. bufnr .. ')'
  augroup END

  return bufnr
enddef

# Submits issue from create buffer
def SubmitIssue(bufnr: number)
  var issue = ParseBuffer(bufnr)

  if empty(issue.title)
    echoerr 'Title is required'
    return
  endif

  var team = getbufvar(bufnr, 'linear_team')
  var project = getbufvar(bufnr, 'linear_project')

  var cmd = LINEAR_CLI .. ' create --team=' .. shellescape(team.id) ..
    ' --title=' .. shellescape(issue.title) ..
    ' --priority=' .. issue.priority

  if !empty(issue.description)
    cmd ..= ' --description=' .. shellescape(issue.description)
  endif

  if type(project) == v:t_dict && has_key(project, 'id')
    cmd ..= ' --project=' .. shellescape(project.id)
  endif

  echo 'Creating issue...'

  var output = system(cmd)
  var exit_code = v:shell_error

  if exit_code == 0
    echo 'Issue created successfully!'
    if !empty(output)
      echo 'Output: ' .. output
    endif
    setbufvar(bufnr, '&modified', 0)
    execute 'bdelete! ' .. bufnr
  else
    echoerr 'Failed to create issue (exit code: ' .. exit_code .. ')'
    if !empty(output)
      echoerr 'Output: ' .. output
    endif
  endif
enddef

# Lists issues for a user-selected project.
export def ListProjectIssues()
  echo 'Fetching projects...'

  var [projects, err] = RunCli('projects --json')
  if !empty(err)
    echoerr 'Error fetching projects: ' .. err
    return
  endif

  if empty(projects)
    echo 'No projects found'
    return
  endif

  var project = ShowMenu(projects, 'Select project:', (item) =>
    item.name .. (has_key(item, 'state') ? (' [' .. item.state .. ']') : ''))

  if type(project) != v:t_dict
    return
  endif

  echo 'Fetching issues for ' .. project.name .. '...'

  var [issues, issues_err] = RunCli('issues ' .. shellescape(project.id) .. ' --json')
  if !empty(issues_err)
    echoerr 'Error fetching issues: ' .. issues_err
    return
  endif

  var bufnr = bufadd('linear-project-issues://' .. substitute(project.name, '[^[:alnum:]-]', '_', 'g'))
  bufload(bufnr)

  setbufvar(bufnr, '&buftype', 'nofile')
  setbufvar(bufnr, '&filetype', 'linear-issues')

  var lines = [
    'PROJECT: ' .. project.name,
    '=' .. repeat('=', len(project.name) + 8),
    '',
  ]

  var issue_map = {}

  if empty(issues)
    add(lines, 'No issues found for this project.')
  else
    for issue in issues
      var identifier = get(issue, 'identifier', '???')
      var title = get(issue, 'title', '(no title)')

      var state_name = 'unknown'
      if has_key(issue, 'state') && type(issue.state) == v:t_dict
        state_name = get(issue.state, 'name', 'unknown')
      elseif has_key(issue, 'state') && type(issue.state) == v:t_string
        state_name = issue.state
      endif

      var assignee_name = 'unassigned'
      if has_key(issue, 'assignee') && type(issue.assignee) == v:t_dict
        assignee_name = get(issue.assignee, 'name', 'unassigned')
      elseif has_key(issue, 'assignee') && type(issue.assignee) == v:t_string
        assignee_name = issue.assignee
      endif

      var priority = get(issue, 'priority', 0)
      var priority_labels = {
        '0': 'None', '1': 'Urgent', '2': 'High', '3': 'Normal', '4': 'Low'
      }
      var priority_str = get(priority_labels, string(priority), string(priority))

      var line_num = len(lines) + 1
      issue_map[line_num] = identifier

      add(lines, printf('%-10s  %s', identifier, title))
      add(lines, printf('            State: %s | Assignee: %s | Priority: %s', state_name, assignee_name, priority_str))
      add(lines, '')
    endfor
  endif

  extend(lines, ['', 'Press <CR> to open issue in new buffer'])

  setbufline(bufnr, 1, lines)
  setbufvar(bufnr, 'linear_issue_map', issue_map)

  augroup LinearIssueList
    autocmd! * <buffer>
    execute 'autocmd! * <buffer=' .. bufnr .. '>'
    execute 'autocmd BufEnter <buffer=' .. bufnr .. '> nnoremap <buffer> <CR> :call ' .. SID .. 'ViewIssueFromMap()<CR>'
  augroup END

  setbufvar(bufnr, '&modifiable', 0)
  execute 'buffer ' .. bufnr
enddef

# Helper to view issue from issue map
def ViewIssueFromMap()
  var issue_map = getbufvar(bufnr('%'), 'linear_issue_map')
  if !empty(issue_map)
    var line_num = line('.')
    if has_key(issue_map, line_num)
      ViewIssue(issue_map[line_num])
    endif
  endif
enddef

# Interactive issue creation with templates
export def CreateIssue()
  echo 'Fetching teams...'

  var [teams, err] = RunCli('teams --json')
  if !empty(err)
    echoerr 'Error fetching teams: ' .. err
    return
  endif

  if empty(teams)
    echo 'No teams found'
    return
  endif

  var team = ShowMenu(teams, 'Select team:', (item) => item.name .. ' (' .. item.key .. ')')
  if type(team) != v:t_dict
    return
  endif

  echo 'Fetching projects...'

  var [projects, proj_err] = RunCli('projects --json')
  if !empty(proj_err)
    echoerr 'Error fetching projects: ' .. proj_err
    return
  endif

  var project_options = [{id: null_string, name: '(No project)'}]
  if !empty(projects)
    extend(project_options, projects)
  endif

  var project = ShowMenu(project_options, 'Select project (optional):', (item) => item.name)
  if type(project) != v:t_dict || !has_key(project, 'id') || empty(project.id)
    project = null_dict
  endif

  echo 'Fetching templates...'

  var [all_templates, tmpl_err] = RunCli('templates --json')
  if !empty(tmpl_err)
    echoerr 'Error fetching templates: ' .. tmpl_err
    return
  endif

  var team_templates = []
  if !empty(all_templates) && type(all_templates) == v:t_dict
    if has_key(all_templates, team.name)
      team_templates = all_templates[team.name]
    endif
  endif

  var template_options = [{id: null_string, name: '(No template)'}]
  extend(template_options, team_templates)

  var template = ShowMenu(template_options, 'Select template (optional):', (item) => item.name)
  if type(template) != v:t_dict || !has_key(template, 'id') || empty(template.id)
    template = null_dict
  endif

  var bufnr = CreateIssueBuffer(team, project, template)
  execute 'buffer ' .. bufnr

  # Position cursor at title line
  cursor(7, 1)
enddef

# Adds a comment using input() prompt
export def AddMinicomment(issue_id: string = '')
  var resolved_id = ResolveIssueId(issue_id)
  if empty(resolved_id)
    return
  endif

  var comment_text = input('Comment text: ')
  if empty(comment_text)
    echo 'Comment text is required'
    return
  endif

  echo 'Adding comment to ' .. resolved_id .. '...'

  var cmd = 'comment ' .. shellescape(resolved_id) .. ' ' .. shellescape(comment_text) .. ' --json'
  var [result, err] = RunCli(cmd)

  if !empty(err)
    echoerr 'Error adding comment: ' .. err
    return
  endif

  var comment_id = has_key(result, 'id') ? result.id : 'unknown'
  echo 'Comment added successfully (ID: ' .. comment_id .. ')'

  # Auto-refresh if viewing this issue
  try
    var buf_issue_id = getbufvar(bufnr('%'), 'linear_issue_id')
    if buf_issue_id == resolved_id
      ViewIssue(resolved_id)
    endif
  catch
    # No refresh needed
  endtry
enddef

# Creates a comment buffer for writing a comment
def CreateCommentBuffer(issue_id: string): number
  var bufnr = bufadd('linear-comment://' .. issue_id)
  bufload(bufnr)

  setbufvar(bufnr, '&buftype', 'acwrite')
  setbufvar(bufnr, '&filetype', 'markdown')

  var lines = [
    '# LINEAR COMMENT',
    '# Issue: ' .. issue_id,
    '',
    '# Write your comment below (markdown supported):',
    '',
    '',
    '# ---',
    '# Save this buffer (:w) to submit the comment',
    '# Close without saving (:q!) to cancel',
  ]

  setbufline(bufnr, 1, lines)
  setbufvar(bufnr, 'linear_comment_issue_id', issue_id)

  augroup LinearComment
    autocmd! * <buffer>
    execute 'autocmd BufWriteCmd <buffer=' .. bufnr .. '> call ' .. SID .. 'SubmitComment(' .. bufnr .. ')'
  augroup END

  return bufnr
enddef

# Submits comment from comment buffer
def SubmitComment(bufnr: number)
  var lines = getbufline(bufnr, 1, '$')
  var issue_id = getbufvar(bufnr, 'linear_comment_issue_id')

  var comment_lines = []
  var in_comment_section = false

  for line in lines
    if line =~# '^# Write your comment below'
      in_comment_section = true
    elseif line =~# '^# ---'
      break
    elseif in_comment_section && line !~# '^#'
      add(comment_lines, line)
    endif
  endfor

  var comment_text = join(comment_lines, "\n")->trim()

  if empty(comment_text)
    echoerr 'Comment text is required'
    return
  endif

  echo 'Adding comment to ' .. issue_id .. '...'

  var cmd = 'comment ' .. shellescape(issue_id) .. ' ' .. shellescape(comment_text) .. ' --json'
  var [result, err] = RunCli(cmd)

  if !empty(err)
    echoerr 'Error adding comment: ' .. err
    return
  endif

  var comment_id = has_key(result, 'id') ? result.id : 'unknown'
  echo 'Comment added successfully (ID: ' .. comment_id .. ')'

  setbufvar(bufnr, '&modified', 0)
  execute 'bdelete! ' .. bufnr

  # Auto-refresh if viewing this issue in another buffer
  for buf in getbufinfo({'buflisted': 1})
    if buf.bufnr != bufnr
      try
        var buf_issue_id = getbufvar(buf.bufnr, 'linear_issue_id')
        if buf_issue_id == issue_id
          ViewIssue(issue_id)
          break
        endif
      catch
        # Continue
      endtry
    endif
  endfor
enddef

# Adds a comment using buffer-based editor
export def AddComment(issue_id: string = '')
  var resolved_id = ResolveIssueId(issue_id)
  if empty(resolved_id)
    return
  endif

  var bufnr = CreateCommentBuffer(resolved_id)
  execute 'buffer ' .. bufnr

  # Position cursor at comment text area
  cursor(6, 1)
enddef

# Parses issue view buffer to extract editable description and comments
def ParseEditableBuffer(bufnr: number): dict<any>
  var lines = getbufline(bufnr, 1, '$')
  var result = {
    description: null_string,
    comments: []
  }

  var current_section = null_string
  var current_comment_id = null_string
  var content_lines = []

  for line in lines
    if line =~# '^<!-- DESCRIPTION START -->'
      current_section = 'description'
      content_lines = []
    elseif line =~# '^<!-- DESCRIPTION END -->'
      if current_section == 'description'
        result.description = join(content_lines, "\n")
      endif
      current_section = null_string
      content_lines = []
    elseif line =~# '^<!-- COMMENT ID: .\+ START -->'
      var comment_id = matchstr(line, '^<!-- COMMENT ID: \zs.\+\ze START -->')
      current_section = 'comment'
      current_comment_id = comment_id
      content_lines = []
    elseif line =~# '^<!-- COMMENT ID: .\+ END -->'
      if current_section == 'comment' && !empty(current_comment_id)
        add(result.comments, {
          id: current_comment_id,
          body: join(content_lines, "\n")
        })
      endif
      current_section = null_string
      current_comment_id = null_string
      content_lines = []
    elseif !empty(current_section)
      if current_section == 'comment'
        # Strip 2-space indentation from comment bodies
        var stripped = substitute(line, '^  ', '', '')
        add(content_lines, stripped)
      else
        add(content_lines, line)
      endif
    endif
  endfor

  return result
enddef

# Saves issue changes from editable buffer
def SaveIssueChanges(bufnr: number)
  var issue_id = getbufvar(bufnr, 'linear_issue_id')
  var original_issue = getbufvar(bufnr, 'linear_issue_data')

  var parsed = ParseEditableBuffer(bufnr)
  var updates = []

  # Check description changes
  if !empty(parsed.description)
    var parsed_desc_norm = parsed.description->trim()
    var original_desc = get(original_issue, 'description', '')
    var original_desc_norm = original_desc->trim()

    if parsed_desc_norm != original_desc_norm
      add(updates, {
        type: 'description',
        text: parsed.description
      })
    endif
  endif

  # Check comment changes
  for parsed_comment in parsed.comments
    var original_body = null_string
    if has_key(original_issue, 'comments') && has_key(original_issue.comments, 'nodes')
      for orig_comment in original_issue.comments.nodes
        if orig_comment.id == parsed_comment.id
          original_body = orig_comment.body
          break
        endif
      endfor
    endif

    var parsed_normalized = parsed_comment.body->trim()
    var original_normalized = !empty(original_body) ? original_body->trim() : ''

    if !empty(original_body) && parsed_normalized != original_normalized
      add(updates, {
        type: 'comment',
        id: parsed_comment.id,
        text: parsed_comment.body
      })
    endif
  endfor

  if empty(updates)
    echo 'No changes to save'
    setbufvar(bufnr, '&modified', 0)
    return
  endif

  # Apply updates sequentially
  for update in updates
    if update.type == 'description'
      echo 'Updating description...'
      var cmd = 'update-description ' .. shellescape(issue_id) .. ' ' .. shellescape(update.text) .. ' --json'
      var [result, err] = RunCli(cmd)
      if !empty(err)
        echoerr 'Error updating description: ' .. err
      else
        echo 'Description updated'
      endif
    elseif update.type == 'comment'
      echo 'Updating comment...'
      var cmd = 'update-comment ' .. shellescape(update.id) .. ' ' .. shellescape(update.text) .. ' --json'
      var [result, err] = RunCli(cmd)
      if !empty(err)
        echoerr 'Error updating comment: ' .. err
      else
        echo 'Comment updated'
      endif
    endif
  endfor

  echo 'All changes saved successfully'
  ViewIssue(issue_id)
enddef

# Views issue with full details and comments (EDITABLE)
export def ViewIssue(issue_id: string = '')
  var resolved_id = ResolveIssueId(issue_id, true)
  if empty(resolved_id)
    return
  endif

  echo 'Fetching issue ' .. resolved_id .. '...'

  var [issue, err] = RunCli('get issue ' .. shellescape(resolved_id) .. ' --json')
  if !empty(err)
    echoerr 'Error fetching issue: ' .. err
    return
  endif

  if empty(issue)
    echoerr 'No issue data returned'
    return
  endif

  # Determine current user name for editable comments
  var current_user_name = null_string
  if has_key(issue, 'assignee') && type(issue.assignee) == v:t_dict && has_key(issue.assignee, 'name')
    current_user_name = issue.assignee.name
  elseif has_key(issue, 'creator') && type(issue.creator) == v:t_dict && has_key(issue.creator, 'name')
    current_user_name = issue.creator.name
  endif

  var buffer_name = 'linear://' .. resolved_id
  var existing_bufnr = bufnr(buffer_name)

  var bufnr: number
  if existing_bufnr != -1
    bufnr = existing_bufnr
    setbufvar(bufnr, '&modifiable', 1)
  else
    bufnr = bufadd(buffer_name)
    bufload(bufnr)
  endif

  setbufvar(bufnr, '&buftype', 'acwrite')
  setbufvar(bufnr, '&filetype', 'markdown')
  setbufvar(bufnr, 'linear_issue_id', resolved_id)
  setbufvar(bufnr, 'linear_issue_data', issue)

  var lines = []
  var identifier = get(issue, 'identifier', resolved_id)
  var title = get(issue, 'title', '(no title)')
  add(lines, '# ' .. identifier .. ': ' .. title)
  add(lines, '')

  if has_key(issue, 'url')
    add(lines, '**URL:** ' .. issue.url)
    add(lines, '')
  endif

  var team_name = 'Unknown'
  if has_key(issue, 'team') && type(issue.team) == v:t_dict
    team_name = get(issue.team, 'name', 'Unknown')
  elseif has_key(issue, 'team') && type(issue.team) == v:t_string
    team_name = issue.team
  endif

  var state_name = 'Unknown'
  if has_key(issue, 'state') && type(issue.state) == v:t_dict
    state_name = get(issue.state, 'name', 'Unknown')
  elseif has_key(issue, 'state') && type(issue.state) == v:t_string
    state_name = issue.state
  endif

  var priority = get(issue, 'priority', 0)
  var priority_labels = {
    '0': 'None', '1': 'Urgent', '2': 'High', '3': 'Normal', '4': 'Low'
  }
  var priority_str = get(priority_labels, string(priority), string(priority))

  add(lines, '**Team:** ' .. team_name)
  add(lines, '**State:** ' .. state_name)
  add(lines, '**Priority:** ' .. priority_str)

  var assignee_name = 'Unassigned'
  if has_key(issue, 'assignee') && type(issue.assignee) == v:t_dict
    assignee_name = get(issue.assignee, 'name', 'Unknown')
  elseif has_key(issue, 'assignee') && type(issue.assignee) == v:t_string
    assignee_name = issue.assignee
  endif
  add(lines, '**Assignee:** ' .. assignee_name)

  if has_key(issue, 'parent') && type(issue.parent) == v:t_dict
    var parent_identifier = get(issue.parent, 'identifier', '???')
    var parent_title = get(issue.parent, 'title', '(no title)')
    add(lines, '**Parent Issue:** [' .. parent_identifier .. '] ' .. parent_title)
  endif

  add(lines, '')

  if has_key(issue, 'createdAt')
    add(lines, '**Created:** ' .. issue.createdAt)
  endif
  if has_key(issue, 'updatedAt')
    add(lines, '**Updated:** ' .. issue.updatedAt)
  endif
  if has_key(issue, 'completedAt')
    add(lines, '**Completed:** ' .. issue.completedAt)
  endif

  extend(lines, ['', repeat('-', 80), '', '## Description', '', '<!-- DESCRIPTION START -->'])

  if has_key(issue, 'description') && !empty(issue.description)
    extend(lines, split(issue.description, "\n", 1))
  else
    add(lines, '(no description)')
  endif

  add(lines, '<!-- DESCRIPTION END -->')
  extend(lines, ['', repeat('-', 80), ''])

  # Sub-issues
  var children_list = []
  if has_key(issue, 'children')
    if type(issue.children) == v:t_dict && has_key(issue.children, 'nodes')
      children_list = issue.children.nodes
    endif
  endif

  if !empty(children_list)
    add(lines, '## Sub-Issues')
    add(lines, '')
    for child in children_list
      var child_identifier = get(child, 'identifier', '???')
      var child_title = get(child, 'title', '(no title)')
      var child_state = 'Unknown'
      if has_key(child, 'state') && type(child.state) == v:t_dict
        child_state = get(child.state, 'name', 'Unknown')
      endif
      add(lines, '- [' .. child_identifier .. '] ' .. child_title .. ' (' .. child_state .. ')')
    endfor
    extend(lines, ['', repeat('-', 80), ''])
  endif

  add(lines, '## Comments')
  add(lines, '')

  var comments_list = []
  if has_key(issue, 'comments')
    if type(issue.comments) == v:t_dict && has_key(issue.comments, 'nodes')
      comments_list = issue.comments.nodes
    elseif type(issue.comments) == v:t_list
      comments_list = issue.comments
    endif
  endif

  if !empty(comments_list)
    var i = 1
    for comment in comments_list
      var author_name = 'Unknown'
      if has_key(comment, 'user') && type(comment.user) == v:t_dict
        author_name = get(comment.user, 'name', 'Unknown')
      elseif has_key(comment, 'user') && type(comment.user) == v:t_string
        author_name = comment.user
      endif

      var timestamp = get(comment, 'createdAt', 'Unknown time')
      var is_user_comment = (!empty(current_user_name) && author_name == current_user_name)

      add(lines, '### Comment ' .. i)
      add(lines, '**' .. author_name .. '** - ' .. timestamp)
      add(lines, '')

      if is_user_comment && has_key(comment, 'id')
        add(lines, '<!-- COMMENT ID: ' .. comment.id .. ' START -->')
      endif

      if has_key(comment, 'body') && !empty(comment.body)
        for line in split(comment.body, "\n", 1)
          add(lines, '  ' .. line)
        endfor
      else
        add(lines, '  (empty comment)')
      endif

      if is_user_comment && has_key(comment, 'id')
        add(lines, '<!-- COMMENT ID: ' .. comment.id .. ' END -->')
      endif

      add(lines, '')
      i += 1
    endfor
  else
    add(lines, '(no comments)')
  endif

  setbufline(bufnr, 1, lines)

  augroup LinearViewIssue
    autocmd! * <buffer>
    execute 'autocmd BufWriteCmd <buffer=' .. bufnr .. '> call ' .. SID .. 'SaveIssueChanges(' .. bufnr .. ')'
  augroup END

  setbufvar(bufnr, '&modifiable', 1)
  execute 'buffer ' .. bufnr

  echo 'Issue ' .. resolved_id .. ' loaded (editable)'
enddef

# Exports issue to markdown file
export def ExportIssue(issue_id: string = '')
  var resolved_id = ResolveIssueId(issue_id)
  if empty(resolved_id)
    return
  endif

  echo 'Exporting issue ' .. resolved_id .. '...'

  var [issue, err] = RunCli('get issue ' .. shellescape(resolved_id) .. ' --json')
  if !empty(err)
    echoerr 'Error fetching issue: ' .. err
    return
  endif

  if empty(issue)
    echoerr 'No issue data returned'
    return
  endif

  var export_dir = expand(config.export_dir)
  var filepath = export_dir .. '/' .. resolved_id .. '.md'

  if !isdirectory(export_dir)
    if !mkdir(export_dir, 'p')
      echoerr 'Failed to create directory: ' .. export_dir
      return
    endif
  endif

  if filereadable(filepath)
    var overwrite = input('File exists. Overwrite? (y/n): ', 'n')
    if overwrite !=? 'y'
      echo 'Export cancelled'
      return
    endif
  endif

  WriteExportFile(issue, resolved_id, filepath)
enddef

# Helper to write export file (no edit markers, all comments included)
def WriteExportFile(issue: dict<any>, resolved_id: string, filepath: string)
  var lines = []
  var identifier = get(issue, 'identifier', resolved_id)
  var title = get(issue, 'title', '(no title)')
  add(lines, '# [' .. identifier .. '] ' .. title)
  add(lines, '')

  if has_key(issue, 'url')
    add(lines, '**URL:** ' .. issue.url)
  endif

  var team_name = 'Unknown'
  if has_key(issue, 'team') && type(issue.team) == v:t_dict
    team_name = get(issue.team, 'name', 'Unknown')
  elseif has_key(issue, 'team') && type(issue.team) == v:t_string
    team_name = issue.team
  endif
  add(lines, '**Team:** ' .. team_name)

  var state_name = 'Unknown'
  if has_key(issue, 'state') && type(issue.state) == v:t_dict
    state_name = get(issue.state, 'name', 'Unknown')
  elseif has_key(issue, 'state') && type(issue.state) == v:t_string
    state_name = issue.state
  endif
  add(lines, '**State:** ' .. state_name)

  var priority = get(issue, 'priority', 0)
  var priority_labels = {
    '0': 'None', '1': 'Urgent', '2': 'High', '3': 'Normal', '4': 'Low'
  }
  var priority_str = get(priority_labels, string(priority), string(priority))
  add(lines, '**Priority:** ' .. priority_str)

  var assignee_name = 'Unassigned'
  if has_key(issue, 'assignee') && type(issue.assignee) == v:t_dict
    assignee_name = get(issue.assignee, 'name', 'Unassigned')
  elseif has_key(issue, 'assignee') && type(issue.assignee) == v:t_string
    assignee_name = issue.assignee
  endif
  add(lines, '**Assignee:** ' .. assignee_name)

  if has_key(issue, 'parent') && type(issue.parent) == v:t_dict
    var parent_identifier = get(issue.parent, 'identifier', '???')
    var parent_title = get(issue.parent, 'title', '(no title)')
    add(lines, '**Parent Issue:** [' .. parent_identifier .. '] ' .. parent_title)
  endif

  if has_key(issue, 'createdAt')
    add(lines, '**Created:** ' .. issue.createdAt)
  endif
  if has_key(issue, 'updatedAt')
    add(lines, '**Updated:** ' .. issue.updatedAt)
  endif
  if has_key(issue, 'completedAt')
    add(lines, '**Completed:** ' .. issue.completedAt)
  endif

  extend(lines, ['', '## Description', ''])

  if has_key(issue, 'description') && !empty(issue.description)
    extend(lines, split(issue.description, "\n", 1))
  else
    add(lines, '(no description)')
  endif

  add(lines, '')

  # Sub-issues
  var children_list = []
  if has_key(issue, 'children')
    if type(issue.children) == v:t_dict && has_key(issue.children, 'nodes')
      children_list = issue.children.nodes
    endif
  endif

  if !empty(children_list)
    add(lines, '## Sub-Issues')
    add(lines, '')
    for child in children_list
      var child_identifier = get(child, 'identifier', '???')
      var child_title = get(child, 'title', '(no title)')
      var child_state = 'Unknown'
      if has_key(child, 'state') && type(child.state) == v:t_dict
        child_state = get(child.state, 'name', 'Unknown')
      endif
      add(lines, '- [' .. child_identifier .. '] ' .. child_title .. ' (' .. child_state .. ')')
    endfor
    add(lines, '')
  endif

  add(lines, '## Comments')
  add(lines, '')

  var comments_list = []
  if has_key(issue, 'comments')
    if type(issue.comments) == v:t_dict && has_key(issue.comments, 'nodes')
      comments_list = issue.comments.nodes
    elseif type(issue.comments) == v:t_list
      comments_list = issue.comments
    endif
  endif

  if !empty(comments_list)
    for comment in comments_list
      var author_name = 'Unknown'
      if has_key(comment, 'user') && type(comment.user) == v:t_dict
        author_name = get(comment.user, 'name', 'Unknown')
      elseif has_key(comment, 'user') && type(comment.user) == v:t_string
        author_name = comment.user
      endif

      var timestamp = get(comment, 'createdAt', 'Unknown time')
      add(lines, '### Comment by ' .. author_name .. ' - ' .. timestamp)
      add(lines, '')

      if has_key(comment, 'body') && !empty(comment.body)
        extend(lines, split(comment.body, "\n", 1))
      else
        add(lines, '(empty comment)')
      endif

      add(lines, '')
    endfor
  else
    add(lines, '(no comments)')
  endif

  if writefile(lines, filepath) == -1
    echoerr 'Failed to write file: ' .. filepath
  else
    echo 'Exported to: ' .. filepath

    var open_file = input('Open file in buffer? (y/n): ', 'y')
    if open_file ==? 'y'
      execute 'edit ' .. fnameescape(filepath)
    endif
  endif
enddef

# Changes issue state
export def ChangeState(issue_id: string = '')
  var resolved_id = ResolveIssueId(issue_id)
  if empty(resolved_id)
    return
  endif

  echo 'Fetching issue ' .. resolved_id .. '...'

  var [issue, err] = RunCli('get issue ' .. shellescape(resolved_id) .. ' --json')
  if !empty(err)
    echoerr 'Error fetching issue: ' .. err
    return
  endif

  if !has_key(issue, 'team') || !has_key(issue.team, 'id')
    echoerr 'Unable to determine team for issue'
    return
  endif

  var team_id = issue.team.id
  echo 'Fetching workflow states for team ' .. issue.team.name .. '...'

  var [states, states_err] = RunCli('states ' .. shellescape(team_id) .. ' --json')
  if !empty(states_err)
    echoerr 'Error fetching workflow states: ' .. states_err
    return
  endif

  if empty(states)
    echo 'No workflow states found for team'
    return
  endif

  var selected_state = ShowMenu(states, 'Select new state:', (item) => item.name .. ' (' .. item.type .. ')')
  if type(selected_state) != v:t_dict
    return
  endif

  echo 'Updating issue ' .. resolved_id .. ' to state ' .. selected_state.name .. '...'

  var cmd = 'update-state ' .. shellescape(resolved_id) .. ' ' .. shellescape(selected_state.id) .. ' --json'
  var [updated_issue, update_err] = RunCli(cmd)

  if !empty(update_err)
    echoerr 'Error updating issue state: ' .. update_err
    return
  endif

  echo 'Issue state updated successfully to: ' .. selected_state.name

  # Auto-refresh if viewing this issue
  try
    var buf_issue_id = getbufvar(bufnr('%'), 'linear_issue_id')
    if buf_issue_id == resolved_id
      ViewIssue(resolved_id)
    endif
  catch
    # No refresh needed
  endtry
enddef

# Assigns issue to a team member
export def AssignIssue(issue_id: string = '')
  var resolved_id = ResolveIssueId(issue_id)
  if empty(resolved_id)
    return
  endif

  echo 'Fetching issue ' .. resolved_id .. '...'

  var [issue, err] = RunCli('get issue ' .. shellescape(resolved_id) .. ' --json')
  if !empty(err)
    echoerr 'Error fetching issue: ' .. err
    return
  endif

  if !has_key(issue, 'team') || !has_key(issue.team, 'id')
    echoerr 'Unable to determine team for issue'
    return
  endif

  var team_id = issue.team.id
  echo 'Fetching team members for ' .. issue.team.name .. '...'

  var [members, members_err] = RunCli('team-members ' .. shellescape(team_id) .. ' --json')
  if !empty(members_err)
    echoerr 'Error fetching team members: ' .. members_err
    return
  endif

  if empty(members)
    echo 'No team members found'
    return
  endif

  var selected_member = ShowMenu(members, 'Select assignee:', (item) => item.name .. ' (' .. item.email .. ')')
  if type(selected_member) != v:t_dict
    return
  endif

  echo 'Assigning issue ' .. resolved_id .. ' to ' .. selected_member.name .. '...'

  var cmd = 'assign ' .. shellescape(resolved_id) .. ' ' .. shellescape(selected_member.id) .. ' --json'
  var [assigned_issue, assign_err] = RunCli(cmd)

  if !empty(assign_err)
    echoerr 'Error assigning issue: ' .. assign_err
    return
  endif

  echo 'Issue assigned successfully to: ' .. selected_member.name

  # Auto-refresh if viewing this issue
  try
    var buf_issue_id = getbufvar(bufnr('%'), 'linear_issue_id')
    if buf_issue_id == resolved_id
      ViewIssue(resolved_id)
    endif
  catch
    # No refresh needed
  endtry
enddef

# Unassigns issue
export def UnassignIssue(issue_id: string = '')
  var resolved_id = ResolveIssueId(issue_id)
  if empty(resolved_id)
    return
  endif

  echo 'Unassigning issue ' .. resolved_id .. '...'

  var cmd = 'unassign ' .. shellescape(resolved_id) .. ' --json'
  var [unassigned_issue, err] = RunCli(cmd)

  if !empty(err)
    echoerr 'Error unassigning issue: ' .. err
    return
  endif

  echo 'Issue unassigned successfully'

  # Auto-refresh if viewing this issue
  try
    var buf_issue_id = getbufvar(bufnr('%'), 'linear_issue_id')
    if buf_issue_id == resolved_id
      ViewIssue(resolved_id)
    endif
  catch
    # No refresh needed
  endtry
enddef

# Takes (self-assigns) an issue
export def TakeIssue(issue_id: string = '')
  var resolved_id = ResolveIssueId(issue_id)
  if empty(resolved_id)
    return
  endif

  echo 'Taking issue ' .. resolved_id .. '...'

  var cmd = 'take ' .. shellescape(resolved_id) .. ' --json'
  var [taken_issue, err] = RunCli(cmd)

  if !empty(err)
    echoerr 'Error taking issue: ' .. err
    return
  endif

  var assignee_name = 'Unknown'
  if has_key(taken_issue, 'assignee') && type(taken_issue.assignee) == v:t_dict
    assignee_name = get(taken_issue.assignee, 'name', 'Unknown')
  endif

  echo 'Issue taken successfully, assigned to: ' .. assignee_name

  # Auto-refresh if viewing this issue
  try
    var buf_issue_id = getbufvar(bufnr('%'), 'linear_issue_id')
    if buf_issue_id == resolved_id
      ViewIssue(resolved_id)
    endif
  catch
    # No refresh needed
  endtry
enddef

# Setup function to configure and register commands
export def Setup(opts: dict<any> = {})
  if has_key(opts, 'export_dir')
    config.export_dir = expand(opts.export_dir)
  endif

  command! -nargs=0 LinearCreate call <SID>CreateIssue()
  command! -nargs=0 LinearIssue call <SID>CreateIssue()
  command! -nargs=0 LinearProjectIssues call <SID>ListProjectIssues()
  command! -nargs=? LinearViewIssue call <SID>ViewIssue(<q-args>)
  command! -nargs=? LinearComment call <SID>AddComment(<q-args>)
  command! -nargs=? LinearMiniComment call <SID>AddMinicomment(<q-args>)
  command! -nargs=? LinearAddComment call <SID>AddComment(<q-args>)
  command! -nargs=? LinearChangeState call <SID>ChangeState(<q-args>)
  command! -nargs=? LinearAssign call <SID>AssignIssue(<q-args>)
  command! -nargs=? LinearUnassign call <SID>UnassignIssue(<q-args>)
  command! -nargs=? LinearTake call <SID>TakeIssue(<q-args>)
  command! -nargs=? LinearExport call <SID>ExportIssue(<q-args>)
enddef

# Auto-setup if not already done
if !exists('g:linear_loaded')
  Setup()
  g:linear_loaded = 1
endif
