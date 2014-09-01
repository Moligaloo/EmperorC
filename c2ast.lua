#!/usr/bin/env lua

package.path = package.path .. ';./lib/?.lua'

local lpeg = require 'lpeg'
local json = require 'dkjson'

local grammar = lpeg.P {
	'units';
	units = lpeg.Ct(lpeg.Cg(lpeg.V 'unit' * lpeg.V 'unit_sep')^1),
	unit_sep = lpeg.S '\n\t ' ^0,
	unit = lpeg.V 'include_directive' + lpeg.V 'func_def',

	include_directive = lpeg.Ct(
		lpeg.Cg(lpeg.Cc 'include', 'type')
		* lpeg.Cg(lpeg.Cp(), 'begin_offset') 
		* '#include' 
		* lpeg.V 'space'  
		* lpeg.V 'inc_filename'
		* lpeg.Cg(lpeg.Cp(), 'end_offset') 
	),
	inc_filename = ('<' * lpeg.V 'filename' * '>') + ('"' * lpeg.V 'filename' * '"'),
	filename = lpeg.Cg((1 - lpeg.S '<>"')^1, 'filename'),

	func_def = lpeg.Ct(
		lpeg.Cg(lpeg.Cc 'func_def', 'type')
		* lpeg.Cg(lpeg.Cp(), 'begin_offset') 
		* lpeg.Cg(lpeg.V 'type', 'return_type') 
		* lpeg.Cg(lpeg.V 'identifier', 'name')
		* lpeg.V 'space0'
		* '('
		* lpeg.Cg(lpeg.V 'param_list', 'param_list')
		* ')'
		* lpeg.V 'space0'
		* '{'
		* lpeg.V 'space0'
		* lpeg.Cg(lpeg.V 'code', 'body')
		* '}'
		* lpeg.Cg(lpeg.Cp(), 'end_offset')
	),
	type = ((lpeg.P 'const' * lpeg.V 'space')^-1 * lpeg.V 'builtin_types' * lpeg.S ' \t*' ^1)
	 / function(s)
	 	return s:gsub("^%s*(.-)%s*$", "%1")
	end,
	identifier = lpeg.V 'letter_' * lpeg.V 'digitletter_' ^0,
	param_def = lpeg.Ct(
		lpeg.V 'space0'
		* lpeg.Cg(lpeg.V 'type', 'type')
		* lpeg.Cg(lpeg.V 'identifier', 'name')
		* lpeg.V 'space0'
	),
	param_list = 
		lpeg.Ct(lpeg.V 'param_def' * (lpeg.P ',' * lpeg.V 'param_def') ^0),

	code = lpeg.Ct(lpeg.V 'statement'),
	statement = lpeg.V 'statement_body' * lpeg.V 'statement_tail',
	statement_tail = lpeg.V 'space0' * ';' * lpeg.V 'space0',
	statement_body = 
		lpeg.V 'return_stmt',

	return_stmt = lpeg.Ct(
		lpeg.Cg(lpeg.Cc 'return_stmt', 'type')
		* lpeg.Cg(lpeg.Cp(), 'begin_offset') 
		* lpeg.P 'return' 
		* lpeg.V 'space' 
		* lpeg.Cg(lpeg.V 'expression', 'expression')
		* lpeg.Cg(lpeg.Cp(), 'end_offset') 
	),
	expression = lpeg.V 'literal',

	space = lpeg.S ' \t\n' ^1,
	space0 = lpeg.S ' \t\n' ^0,
	builtin_types = lpeg.P 'int' + lpeg.P 'char',
	control_flow = lpeg.P 'if' + lpeg.P 'while' + lpeg.P 'for' + lpeg.P 'do',

	digit = lpeg.R '09',
	nonzerodigit = lpeg.R '19',
	hexdigit = lpeg.V 'digit' + lpeg.R 'af' + lpeg.R 'AF',
	letter = lpeg.R 'az' + lpeg.R 'AZ',
	letter_ = lpeg.V 'letter' + '_',
	digitletter_ = lpeg.V 'digit' + lpeg.V 'letter_',

	decimal = 
		('0' * #(1-lpeg.V 'digit'- 'x'))
		+ (lpeg.V "nonzerodigit" * lpeg.V "digit" ^0) * #(1-lpeg.V 'digit'),
	hexadecimal = lpeg.P "0x" * lpeg.V'hexdigit' ^ 1,
	integer = lpeg.V 'hexadecimal' + lpeg.V 'decimal',
	float = lpeg.V 'decimal' * '.' * lpeg.V 'digit' ^1,

	literal = 
		lpeg.V 'float' / function(s)
			return {type = 'float', value = tonumber(s)}
		end
		+lpeg.V 'integer' / function(s) 
			return {type = "integer", value = tonumber(s)}
		 end,
}

function c2ast(source)
	local fp = io.open(source, "r")
	local content = fp:read '*a'
	fp:close()

	local ast = grammar:match(content)
	print(json.encode(ast, {indent = true}))
end

c2ast(arg[1] or "./example/hello.c")

