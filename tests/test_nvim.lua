-- Test script for Linear Neovim plugin
-- Run with: nvim --headless -u NONE -c 'luafile test_nvim.lua'

-- Add the linear-cli directory to package path
package.path = package.path .. ";/Users/jaesaxon/hc/linear-cli/?.lua"

-- Load the Linear plugin
local linear = require("linear")

-- Setup the plugin
linear.setup()

-- Test viewing issue SRE-4
print("\n=== Testing M.view_issue('SRE-4') ===\n")

-- Since we're in headless mode, we need to wait for async operations
-- Let's just call it and let vim handle the async stuff
linear.view_issue("SRE-4")

-- Give it time to execute
vim.defer_fn(function()
  print("\n=== Test completed ===")
  vim.cmd('quit!')
end, 3000)
