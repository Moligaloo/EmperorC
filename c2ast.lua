#!/usr/bin/env lua

package.path = package.path .. ';./lib/?.lua'

local lpeg = require 'lpeg'
local json = require 'dkjson'

local grammar = lpeg.P {
	'units';
	units = lpeg.Ct(lpeg.Cg(lpeg.V 'unit' * lpeg.V 'unit_sep')^1),
	unit_sep = lpeg.S '\n\t ' ^0,
	unit = 
		lpeg.V 'include_directive' 
		+ lpeg.V 'func_def',

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

	code = lpeg.Ct(lpeg.V 'statement' ^0),
	statement = lpeg.V 'statement_body' * lpeg.V 'statement_tail',
	statement_tail = lpeg.V 'space0' * ';' * lpeg.V 'space0',
	statement_body = 
		lpeg.V 'return_stmt'
		+ lpeg.V 'expression_stmt',

	return_stmt = lpeg.Ct(
		lpeg.Cg(lpeg.Cc 'return_stmt', 'type')
		* lpeg.Cg(lpeg.Cp(), 'begin_offset') 
		* lpeg.P 'return' 
		* lpeg.V 'space' 
		* lpeg.Cg(lpeg.V 'expression', 'expression')
		* lpeg.Cg(lpeg.Cp(), 'end_offset') 
	),
	expression_stmt = lpeg.Ct(
		lpeg.Cg(lpeg.Cc 'expression_stmt', 'type')
		* lpeg.Cg(lpeg.Cp(), 'begin_offset') 
		* lpeg.Cg(lpeg.V 'expression', 'expression')
		* lpeg.Cg(lpeg.Cp(), 'end_offset') 
	),

	expression = 
		lpeg.V 'literal'
		+ lpeg.V 'func_call',

	space = lpeg.S ' \t\n' ^1,
	space0 = lpeg.S ' \t\n' ^0,
	builtin_types = lpeg.P 'int' + lpeg.P 'char',
	control_flow = lpeg.P 'if' + lpeg.P 'while' + lpeg.P 'for' + lpeg.P 'do',

	func_call = lpeg.Ct(
		lpeg.Cg(lpeg.Cc 'func_call', 'type')
		* lpeg.Cg(lpeg.V 'identifier', 'func_name')
		* lpeg.V 'space0'
		* '('
		* lpeg.V 'space0'
		* lpeg.Cg(lpeg.V 'argument_list', 'argument_list')
		* lpeg.V 'space0'
		* ')'
	),
	argument_list = lpeg.Ct(
		lpeg.V 'expression'
		* (lpeg.V 'spacecomma' * lpeg.V 'expression')^0
	)
		+ (lpeg.Cc {}), 
	spacecomma = lpeg.V 'space0' * ',' * lpeg.V 'space0',

	digit = lpeg.R '09',
	nonzerodigit = lpeg.R '19',
	hexdigit = lpeg.R('09','af', 'AF'),
	octaldigit = lpeg.R('07'),
	letter = lpeg.R('az', 'AZ'),
	letter_ = lpeg.V 'letter' + '_',
	digitletter_ = lpeg.V 'digit' + lpeg.V 'letter_',

	decimal = 
		('0' * #(1-lpeg.V 'digit'- 'x'))
		+ (lpeg.V "nonzerodigit" * lpeg.V "digit" ^0) * #(1-lpeg.V 'digit'),
	hexadecimal = ('0' * lpeg.S 'xX') * lpeg.V'hexdigit' ^ 1,
	octal = '0' * (lpeg.V 'octaldigit')^1,
	integer = 
		lpeg.V 'hexadecimal' / tonumber
		+ lpeg.V 'octal' / function(s) return tonumber(s, 8) end
		+ lpeg.V 'decimal' / tonumber,
	float = lpeg.V 'decimal' * '.' * lpeg.V 'digit' ^1,

	char_in_string = 1 - lpeg.P '"',
	string = '"' * lpeg.V 'char_in_string' ^0 * '"',

	literal = 
		lpeg.V 'float' / function(s)
			return {type = 'float', value = tonumber(s)}
		end
		+ lpeg.V 'integer' / function(v) 
			return { type = "integer", value = v} 
		end
		+ lpeg.V 'string' / function(s)
			s = s:gsub('"(.+)"', "%1") 
			
			s = s:gsub('\\"', '"')
			s = s:gsub("\\'", "'")
			s = s:gsub('\\\\', '\\')
			s = s:gsub('\\a', '\a')
			s = s:gsub('\\b', '\b')
			s = s:gsub('\\f', '\f')
			s = s:gsub('\\n', "\n")
			s = s:gsub('\\r', '\r')
			s = s:gsub('\\t', '\t')
			s = s:gsub('\\v', '\v')

			return {type = "string", value = s}
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

