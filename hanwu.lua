#!/usr/bin/env lua

local action_name = arg[1]
local source = arg[2]
local target = arg[3]

local function printf(format, ...)
  print(format:format(...))
end

if action_name == nil or source == nil then
  printf("Usage: %s <action> <source> <optional target>", arg[0])
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

local function basename(filename)
  local basename = filename
  for pattern, _ in pairs(pattern_to_type) do
    basename = basename:gsub(pattern, '')
  end
  return basename
end

local function filename_with_type(filename, type)
  return basename(filename) .. type_to_extension[type]
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
    target = target or filename_with_type(source, 'ast')
    session:to_ast(target)
  end, 

  decompile = function()
    local session = emperor.session.new()
    expect_type(source, {'ast'})
    session:load_ast(source)
    target = target or filename_with_type(source, 'c')
    session:to_c(target)
  end,
}

local action = actions[action_name]
if action then
  action()
else
  printf("Unsupported action %s", action_name)
end

