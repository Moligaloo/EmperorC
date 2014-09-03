#!/usr/bin/env lua

package.path = package.path .. ';./lib/?.lua'

local re = require 're'
local json = require 'dkjson'

local defs = {
	simplify_type = function(s)
		return s:gsub("^%s*(.-)%s*$", "%1"):gsub("%s+", ' ')
	end,
	tonumber = tonumber,
	tonumber8 = function(s) 
		return tonumber(s, 8)
	end,
	unquote = function(s)
		return s:gsub('\\"', '"')
				:gsub("\\'", "'")
				:gsub('\\\\', '\\')
				:gsub('\\a', '\a')
				:gsub('\\b', '\b')
				:gsub('\\f', '\f')
				:gsub('\\n', "\n")
				:gsub('\\r', '\r')
				:gsub('\\t', '\t')
				:gsub('\\v', '\v')
	end,
}

local grammar_string = [[
	units <- {| (S0 unit S0)+ |}
 	unit <- include_directive / func_def
	include_directive <- {| @include@ <<< '#include' S0 '<' {:filename: filename :} '>' >>> |} 
	filename <- {[^"<>]+}
	type <- identifier
	func_def <- {|
		@func_def@
		<<<
		{:return_type:type:} S {:name:identifier:} S0 
		'(' {:params: {| params |} :} ')' S0
		'{' S0 {:code:code:} S0 '}'
		>>>
	|}
	identifier <- [a-zA-Z_][a-zA-Z0-9_]*
	params <- (param_def (S0 ',' S0 param_def)* )?
	param_def <- {| {:type:param_type:} S0 {:name:identifier:} |} 
	param_type <- {'const'? S0 type S0 ('*'+ 'const'?  / '') &[_a-zA-Z ] } -> simplify_type
	code <- {| (S0 statement S0) + |}
	statement <- expression_stmt 
	expression_stmt <- 
		{| {:expression:expression:} S0 ';' |}
	expression <-
		{| @float@ {:value:float:} |} 
		/ {| @integer@ {:value:integer:} |}
		/ {| @string@ {:value:string:} |}

	S <- %s+
	S0 <- %s*

	nzdigit <- [1-9]
	decimal <- ('0' / nzdigit %d*) !%d
	hexdigit <- ('0x' / '0X') %x+
	octal <- '0' [0-7]+
	integer <-  
		{hexdigit} -> tonumber 
		/ {octal} -> tonumber8 
		/ {decimal} -> tonumber

	float <- {decimal '.' %d+ } -> tonumber

	char_in_string <- ( '\' [abfntf"] ) / [^"]
	string <- ('"' {char_in_string+} '"' ) -> unquote
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

