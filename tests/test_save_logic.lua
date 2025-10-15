-- Test to verify save logic only updates changed sections
-- Run with: lua test_save_logic.lua

-- Mock the comparison logic from linear.lua
local function compare_sections(original_issue, parsed)
  local updates = {}

  -- Check if description changed
  if parsed.description then
    -- Normalize whitespace for comparison (trim trailing/leading)
    local parsed_desc_norm = parsed.description:gsub("^%s+", ""):gsub("%s+$", "")
    local original_desc_norm = (original_issue.description or ""):gsub("^%s+", ""):gsub("%s+$", "")

    if parsed_desc_norm ~= original_desc_norm then
      table.insert(updates, {
        type = "description",
        text = parsed.description
      })
    end
  end

  -- Check if comments changed
  for _, parsed_comment in ipairs(parsed.comments) do
    -- Find the original comment
    local original_body = nil
    if original_issue.comments and original_issue.comments.nodes then
      for _, orig_comment in ipairs(original_issue.comments.nodes) do
        if orig_comment.id == parsed_comment.id then
          original_body = orig_comment.body
          break
        end
      end
    end

    -- Normalize whitespace for comparison (trim trailing/leading, normalize newlines)
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

  return updates
end

-- Test cases
print("Running save logic tests...\n")

-- Test 1: No changes
print("Test 1: No changes should result in 0 updates")
local original_1 = {
  description = "This is the description",
  comments = {
    nodes = {
      {id = "comment-1", body = "First comment"},
      {id = "comment-2", body = "Second comment"}
    }
  }
}
local parsed_1 = {
  description = "This is the description",
  comments = {
    {id = "comment-1", body = "First comment"},
    {id = "comment-2", body = "Second comment"}
  }
}
local updates_1 = compare_sections(original_1, parsed_1)
print(string.format("  Result: %d updates (expected 0) - %s", #updates_1, #updates_1 == 0 and "PASS" or "FAIL"))

-- Test 2: Only description changed
print("\nTest 2: Only description changed should result in 1 update")
local original_2 = {
  description = "Original description",
  comments = {
    nodes = {
      {id = "comment-1", body = "First comment"}
    }
  }
}
local parsed_2 = {
  description = "Modified description",
  comments = {
    {id = "comment-1", body = "First comment"}
  }
}
local updates_2 = compare_sections(original_2, parsed_2)
print(string.format("  Result: %d updates (expected 1) - %s", #updates_2, #updates_2 == 1 and "PASS" or "FAIL"))
if #updates_2 == 1 then
  print(string.format("  Update type: %s", updates_2[1].type))
end

-- Test 3: Only one comment changed
print("\nTest 3: Only one comment changed should result in 1 update")
local original_3 = {
  description = "Description",
  comments = {
    nodes = {
      {id = "comment-1", body = "First comment"},
      {id = "comment-2", body = "Second comment"}
    }
  }
}
local parsed_3 = {
  description = "Description",
  comments = {
    {id = "comment-1", body = "First comment"},
    {id = "comment-2", body = "Modified second comment"}
  }
}
local updates_3 = compare_sections(original_3, parsed_3)
print(string.format("  Result: %d updates (expected 1) - %s", #updates_3, #updates_3 == 1 and "PASS" or "FAIL"))
if #updates_3 == 1 then
  print(string.format("  Update type: %s, comment ID: %s", updates_3[1].type, updates_3[1].id))
end

-- Test 4: Whitespace differences only (should not trigger update)
print("\nTest 4: Whitespace differences only should result in 0 updates")
local original_4 = {
  description = "Description text",
  comments = {
    nodes = {
      {id = "comment-1", body = "Comment text"}
    }
  }
}
local parsed_4 = {
  description = "  Description text  ",  -- Added leading/trailing whitespace
  comments = {
    {id = "comment-1", body = "  Comment text  "}  -- Added leading/trailing whitespace
  }
}
local updates_4 = compare_sections(original_4, parsed_4)
print(string.format("  Result: %d updates (expected 0) - %s", #updates_4, #updates_4 == 0 and "PASS" or "FAIL"))

-- Test 5: Both description and comment changed
print("\nTest 5: Both description and comment changed should result in 2 updates")
local original_5 = {
  description = "Original",
  comments = {
    nodes = {
      {id = "comment-1", body = "Original comment"}
    }
  }
}
local parsed_5 = {
  description = "Modified",
  comments = {
    {id = "comment-1", body = "Modified comment"}
  }
}
local updates_5 = compare_sections(original_5, parsed_5)
print(string.format("  Result: %d updates (expected 2) - %s", #updates_5, #updates_5 == 2 and "PASS" or "FAIL"))
if #updates_5 == 2 then
  print(string.format("  Update 1 type: %s", updates_5[1].type))
  print(string.format("  Update 2 type: %s", updates_5[2].type))
end

-- Test 6: Multiple comments, only middle one changed
print("\nTest 6: Multiple comments, only middle one changed should result in 1 update")
local original_6 = {
  description = "Description",
  comments = {
    nodes = {
      {id = "comment-1", body = "First"},
      {id = "comment-2", body = "Second"},
      {id = "comment-3", body = "Third"}
    }
  }
}
local parsed_6 = {
  description = "Description",
  comments = {
    {id = "comment-1", body = "First"},
    {id = "comment-2", body = "Modified second"},
    {id = "comment-3", body = "Third"}
  }
}
local updates_6 = compare_sections(original_6, parsed_6)
print(string.format("  Result: %d updates (expected 1) - %s", #updates_6, #updates_6 == 1 and "PASS" or "FAIL"))
if #updates_6 == 1 then
  print(string.format("  Update type: %s, comment ID: %s", updates_6[1].type, updates_6[1].id))
end

print("\n=== Test Summary ===")
local all_tests = {updates_1, updates_2, updates_3, updates_4, updates_5, updates_6}
local expected = {0, 1, 1, 0, 2, 1}
local passed = 0
for i = 1, #all_tests do
  if #all_tests[i] == expected[i] then
    passed = passed + 1
  end
end
print(string.format("Passed: %d/%d tests", passed, #all_tests))
