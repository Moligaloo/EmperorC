#!/usr/bin/env lua

package.path = package.path .. ';./lib/?.lua'

local re = require 're'
local json = require 'dkjson'

local defs = {
	simplify_type = function(s)
		return s:gsub("^%s*(.-)%s*$", "%1"):gsub("%s+", ' ')
	end
}

local grammar_string = [[
	units <- {| (S0 unit S0)+ |}
 	unit <- include_directive / func_def
	include_directive <- {| @include@ <<< '#include' S0 '<' {:filename: filename :} '>' >>> |} 
	filename <- {[^"'<>]+}
	type <- identifier
	func_def <- {|
		@func_def@
		<<<
		{:return_type:type:} S {:name:identifier:} S0 
		'(' {:params: {| params |} :} ')' 
		>>>
	|}
	identifier <- [a-zA-Z_][a-zA-Z0-9_]*
	params <- (param_def (S0 ',' S0 param_def)* )?
	param_def <- {| {:type:param_type:} S0 {:name:identifier:} |} 
	param_type <- {'const'? S0 type S0 ('*'+ 'const'?  / '') &[_a-zA-Z ] } -> simplify_type

	S <- %s+
	S0 <- %s*
]]

grammar_string = grammar_string
	:gsub("<<<", "{:start_offset: {} :}" )
	:gsub(">>>", "{:end_offset: {} :}")
	:gsub("@([%w_]+)@", "{:type: '' -> '%1' :}")

local grammar = re.compile(grammar_string, defs)

function c2ast(source)
	local fp = io.open(source, "r")
	local content = fp:read '*a'
	fp:close()

	local ast = grammar:match(content)
	print(json.encode(ast, {indent = true}))
end

c2ast(arg[1] or "./example/hello.c")

