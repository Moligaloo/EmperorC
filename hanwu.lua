#!/usr/bin/env lua

local action_name = arg[1]
local source = arg[2]

local function guess_type(filename)
	if filename:match '.c$' then
		return 'c'
	elseif filename:match '.ast.json$' then
		return 'ast'
	elseif filename:match '.asm.json$' then
		return 'asm'
	end
end

local function printf(format, ...)
	print(format:format(...))
end

local actions = {
-- compile .c source code to .ast.json file
	to_ast = function()
	end,

-- compile .c or .ast.json file to .asm.json file
	to_asm = function()
	end,

-- directly run c, ast or asm file
	run = function()
	end,

-- debug source code
	debug = function()
	end,
}

if action_name and source then
	local action = actions[action_name]
	if action then
		action()
	else
		printf("Unsupported action %s", action_name)
	end
else
	printf("Usage: %s <action> <source>", arg[0])
end
