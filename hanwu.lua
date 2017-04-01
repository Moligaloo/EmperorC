#!/usr/bin/env lua

local action_name = arg[1]
local source = arg[2]
local target = arg[3]

local function printf(format, ...)
  print(format:format(...))
end

if action_name == nil or source == nil then
  printf("Usage: %s <action> <source>", arg[0])
  return
end

package.path = package.path .. ";./?/init.lua"

local emperor = require 'emperor_c'

local pattern_to_type = {
  ['%.c$'] = 'c',
  ['%.json$'] = 'ast'
}

local type_to_extension = {
  c = '.c',
  ast = '.json'
}

local function guess_type(filename)
  for pattern, type in pairs(pattern_to_type) do
    if filename:match(pattern) then
      return type
    end
  end
end

local function expect_type(filename, expected_types)
  local guessed = guess_type(filename)
  if guessed == nil then
    error(("%s's type is not supported"):format(filename))
  end

  for _, type in ipairs(expected_types) do
    if type == guessed then
      return 
    end
  end

  error(("%s's type %s is not supported!"):format(filename, guessed))
end

local actions = {
-- compile C to AST tree
  compile = function()
    local session = emperor.session.new()
    expect_type(source, {'c'})
    session:load_c(source)
    print(session:to_ast())
  end, 

  decompile = function()
    local session = emperor.session.new()
    expect_type(source, {'ast'})
    session:load_ast(source)
    print(session:to_c())
  end,
}

local action = actions[action_name]
if action then
  action()
else
  printf("Unsupported action %s", action_name)
end

