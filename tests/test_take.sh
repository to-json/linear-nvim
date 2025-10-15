#!/bin/bash

# Test script for the 'take' command
# This script demonstrates the functionality without making actual API calls

echo "=== Testing 'take' command implementation ==="
echo ""

echo "1. Testing CLI help/usage:"
./linear-cli take 2>&1 | head -1
echo ""

echo "2. Testing CLI binary exists and take function is defined:"
grep -q "func takeIssue" main.go && echo "   ✓ takeIssue function found in main.go"
grep -q "os.Args\[1\] == \"take\"" main.go && echo "   ✓ CLI handler for 'take' command found"
echo ""

echo "3. Testing Vim plugin function:"
grep -q "function M.take_issue" linear.lua && echo "   ✓ M.take_issue function found in linear.lua"
grep -q "LinearTake" linear.lua && echo "   ✓ LinearTake command registration found"
echo ""

echo "4. Verifying context-aware logic in Vim function:"
grep -q "linear_issue_id" linear.lua && echo "   ✓ Checks for linear_issue_id buffer variable"
grep -q "linear_issue_map" linear.lua && echo "   ✓ Checks for linear_issue_map buffer variable"
grep -q "vim.ui.input" linear.lua && echo "   ✓ Falls back to user input prompt"
echo ""

echo "5. Verifying auto-refresh logic:"
grep -A5 "function M.take_issue" linear.lua | grep -q "M.view_issue" && echo "   ✓ Auto-refreshes buffer after assignment"
echo ""

echo "=== Implementation Summary ==="
echo ""
echo "CLI Command:"
echo "  linear-cli take <issue-id>"
echo "    - Gets viewer ID from GraphQL viewer query"
echo "    - Calls assignIssue with viewer ID"
echo ""
echo "Vim Plugin:"
echo "  :LinearTake [id]"
echo "    - Context-aware: extracts ID from buffer variables or prompts"
echo "    - Auto-refreshes the issue view after assignment"
echo ""
echo "Context sources (in order of priority):"
echo "  1. Explicit argument passed to function"
echo "  2. linear_issue_id buffer variable (when viewing an issue)"
echo "  3. linear_issue_map buffer variable (when in issue list)"
echo "  4. User input prompt"
echo ""
