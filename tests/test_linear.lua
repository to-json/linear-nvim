-- Test script for Linear Neovim plugin
-- Run with: nvim --headless -u test_linear.lua

-- Add the linear-cli directory to package path
package.path = package.path .. ";/Users/jaesaxon/hc/linear-cli/?.lua"

-- Mock vim API for testing
_G.vim = {
  fn = {
    expand = function(path)
      return path:gsub("$HOME", os.getenv("HOME"))
    end,
    shellescape = function(str)
      return "'" .. str:gsub("'", "'\\''") .. "'"
    end,
    jobstart = function(cmd, opts)
      print("Running command: " .. cmd)
      -- Execute the command synchronously for testing
      local handle = io.popen(cmd)
      local result = handle:read("*a")
      handle:close()

      if opts.stdout_buffered and opts.on_stdout then
        local lines = {}
        for line in result:gmatch("[^\n]+") do
          table.insert(lines, line)
        end
        opts.on_stdout(nil, lines)
      end

      if opts.on_exit then
        opts.on_exit(nil, 0)
      end

      return 1
    end,
  },
  api = {
    nvim_create_buf = function() return 1 end,
    nvim_buf_set_option = function() end,
    nvim_buf_set_var = function() end,
    nvim_buf_set_lines = function(buf, start, end_, strict, lines)
      print("\n=== BUFFER CONTENT ===")
      for i, line in ipairs(lines) do
        print(string.format("%3d: %s", i, line))
      end
      print("=== END BUFFER ===\n")
    end,
    nvim_set_current_buf = function() end,
    nvim_get_current_buf = function() return 1 end,
    nvim_buf_get_var = function() error("no var") end,
    nvim_create_user_command = function() end,
  },
  ui = {
    input = function(opts, callback)
      print("Input prompt: " .. opts.prompt)
      -- For testing, provide the issue ID
      callback("SRE-4")
    end,
    select = function(items, opts, callback)
      print("Select prompt: " .. opts.prompt)
      for i, item in ipairs(items) do
        print(string.format("  %d. %s", i, opts.format_item(item)))
      end
      -- For testing, select the first item
      if #items > 0 then
        callback(items[1])
      end
    end,
  },
  notify = function(msg, level)
    local levels = {
      [0] = "ERROR",
      [1] = "WARN",
      [2] = "INFO",
      [3] = "DEBUG",
    }
    print(string.format("[%s] %s", levels[level] or "INFO", msg))
  end,
  log = {
    levels = {
      DEBUG = 3,
      INFO = 2,
      WARN = 1,
      ERROR = 0,
    },
  },
  schedule = function(fn) fn() end,
  list_extend = function(list, values)
    for _, v in ipairs(values) do
      table.insert(list, v)
    end
    return list
  end,
  json = {
    decode = function(str)
      -- Use Lua's built-in JSON parser or a simple implementation
      local json = require("json") or loadfile("json.lua")()
      return json.decode(str)
    end,
  },
}

-- Simple JSON decoder for testing (fallback)
local function json_decode(str)
  -- Try using dkjson if available, otherwise use a basic parser
  local ok, dkjson = pcall(require, "dkjson")
  if ok then
    return dkjson.decode(str)
  end

  -- Fallback: try using lua-cjson
  local ok2, cjson = pcall(require, "cjson")
  if ok2 then
    return cjson.decode(str)
  end

  -- Last resort: eval the JSON (NOT SAFE, only for testing)
  local fn = load("return " .. str:gsub("null", "nil"):gsub(":true", ":true"):gsub(":false", ":false"))
  if fn then return fn() end

  error("No JSON library available")
end

-- Override vim.json.decode to use our fallback
vim.json.decode = json_decode

-- Load the Linear plugin
local linear = require("linear")

-- Test viewing issue SRE-4
print("\n=== Testing M.view_issue('SRE-4') ===\n")
linear.view_issue("SRE-4")

print("\n=== Test completed ===")
