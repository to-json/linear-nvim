-- Test JSON parsing
local json_str = vim.fn.system('./linear-cli get issue BADID-1 --json')
local issue = vim.json.decode(json_str)

print("Issue ID:", issue.identifier)
print("Has children field:", issue.children ~= nil)
if issue.children then
  print("Children type:", type(issue.children))
  print("Has nodes:", issue.children.nodes ~= nil)
  if issue.children.nodes then
    print("Nodes type:", type(issue.children.nodes))
    print("Nodes length:", #issue.children.nodes)
    for i, child in ipairs(issue.children.nodes) do
      print(string.format("  Child %d: %s - %s", i, child.identifier, child.title))
    end
  end
end
