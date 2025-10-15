-- Drop this into ~/.config/nvim/lua/plugins/ to load our custom plugin, replacing the path with wherever you have this extracted
return {
  dir = "~/hc/linear-cli",
  name = "linear",
  config = function()
    -- Add the lua directory to package path
    package.path = package.path .. ";~/hc/linear-cli/?.lua"
    require("linear").setup()
  end,
  keys = {
    { "<leader>Tc", "<cmd>LinearCreate<cr>",        desc = "Create Linear issue" },
    { "<leader>Tp", "<cmd>LinearProjectIssues<cr>", desc = "List project issues" },
    { "<leader>Tv", "<cmd>LinearViewIssue<cr>",     desc = "View Linear issue" },
    { "<leader>Tm", "<cmd>LinearAddComment<cr>",    desc = "Add comment" },
    { "<leader>Ts", "<cmd>LinearChangeState<cr>",   desc = "Change state" },
    { "<leader>Ta", "<cmd>LinearAssign<cr>",        desc = "Assign issue" },
    { "<leader>Tu", "<cmd>LinearUnassign<cr>",      desc = "Unassign issue" },
  },
}
