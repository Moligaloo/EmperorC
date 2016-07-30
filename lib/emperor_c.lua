local re = require 're'

-- copied from https://en.wikipedia.org/wiki/Escape_sequences_in_C
local escaped_char_map = {
	a = 0x07,
	b = 0x08,
	f = 0x0C,
	n = 0x0A,
	r = 0x0D,
	t = 0x09,
	v = 0x0B,
	["'"] = 0x27,
	['"'] = 0x22
}

local reverse_escaped_char_map = {}
for key, value in pairs(escaped_char_map) do
	reverse_escaped_char_map[value] = key
end

local function readable_char(char, type)
	local str = string.char(char)

	if str == "'" and type == 'char' then
		return [[\']]
	elseif str == '"' and type == 'string' then
		return [[\"]]
	end

	if str:find("%g") then
		return ("%s"):format(str)
	else
		if reverse_escaped_char_map[char] then
			return ("\\%s"):format(reverse_escaped_char_map[char])
		end

		return ("\\x%02X"):format(char)
	end
end

local metatables = {
	integer = {
		__index = {
			tostring = function(self)
				return ("%d"):format(self.value)
			end,
			size = function() return 4 end
		}
	},
	float = {
		__index = {
			tostring = function(self)
				return ("%g"):format(self.value)
			end,
			size = function() return 4 end
		}
	},
	character = {
		__index = {
			tostring = function(self)
				return ("'%s'"):format(readable_char(self.value, 'char'))
			end,
			size = function() return 1 end
		}
	},
	string = {
		__index = {
			tostring = function(self)
				return ('"%s"'):format(self.value:gsub(".", function(char)
					if char == '"' then
						return '\\"'
					end

					if char:find "%g" then
						return char
					else
						local byte = string.byte(char)
						if reverse_escaped_char_map[byte] then
							return "\\" .. reverse_escaped_char_map[byte]
						else
							return ("\\x%02X"):format(byte)
						end
					end
				end))
			end,
			size = function() return #self.value + 1 end
		}
	}
}

local function create_value(type, value)
	return setmetatable({type = type, value = value}, metatables[type])
end

local grammar = re.compile([[
	definitions <- {| (S definition S)+ |}
	definition <- function_definition / global_variable_definition
	global_variable_definition <- {| {:definition: '' -> 'global' :} <vardef> |} 
	static_initializer <- '=' S {: literal_value :}
	literal_value <- float / integer / character / string
	integer <- hexadecimal_integer / decimal_integer
	hexadecimal_integer <- ('0x' HEXCHAR+) -> hexadecimal_integer
	decimal_integer <- (%d+) -> decimal_integer
	float <- (%d+ '.' %d+) -> float
	character <- "'" single_character "'"
	single_character <- ('\' { [abfnrtv'] } ) -> escaped_char / [^'] -> normal_char 
	string <- '"' {| char_in_string* |} -> string '"'
	char_in_string <- ('\' {[abfnrtv"]} ) -> escaped_char_map / [^"] -> string_byte

	function_definition <- {| {:definition:'' -> 'function' :} <function_head> {:body:function_body:} |}
	function_head <- {:return_type:RETURN_TYPE:} S {:name:IDENTIFIER:} S '(' S {:parameters:parameters:} S ')'
	function_body <- {| S compound_statement S |}
	expression <- call_expression / unary_expression / binary_expression / term
	unary_expression <- {| {:op: UNUARY_OP :} S {:A: term :} |}
	binary_expression <- {| {:A: term :} S {:op: BINARY_OP :} S {:B: term :} |}
	call_expression <- {| {:function_name:IDENTIFIER:} S {:arguments: '(' S {: arguments :} S ')' :} |}

	-- statements
	statement <- compound_statement / jump_statement / expression_statement / vardef_statement
	compound_statement <- '{' S statement* S '}'
	expression_statement <- {| {:statement: '' -> 'expression' :} {:expression: expression :} ENDING_SEMICOLON |}
	
	jump_statement <- {| {:statement: '' -> 'jump' :} <jump_action> ENDING_SEMICOLON |}
	jump_action <- jump_goto / jump_continue / jump_break / jump_return
	jump_goto <- {:jump: 'goto' :} S {:label: IDENTIFIER :}
	jump_continue <- {:jump: 'continue' :}
	jump_break <- {:jump: 'break' :}
	jump_return <- {:jump: 'return' :} S {:value: expression :}?

	vardef_statement <- {| {:statement: '' -> 'vardef' :} <vardef> |} 
	term <- literal_value / variable
	variable <- {| {:name: IDENTIFIER :} |} -> variable
	arguments <- {| argument (S ',' S argument)* |} / ''
	argument <- expression
	parameter <- {| {:type: VAR_TYPE :} S {:quad:vardef_quad:} |} -> parameter
	parameters <- {| parameter (S ',' S parameter)* |} / '' 

	vardef_stars <- {:stars: [*]+ :}
	vardef_name <- {:name: IDENTIFIER :}
	vardef_bracket <- {:array_count: '[' integer ']' :}
	vardef_initializer <- '=' S {:initializer: expression :}
	vardef_quad <- {| <vardef_stars>? S <vardef_name> S <vardef_bracket>? S <vardef_initializer>? |}
	vardef_quads <- {| vardef_quad (S ',' S vardef_quad)* |}
	vardef_modifier <- S {STORAGE} S
	vardef_modifiers <- {| vardef_modifier+ |}
	vardef <-  S {:modifiers:vardef_modifiers:}? S {:type: VAR_TYPE :} S {:quads: vardef_quads :} ENDING_SEMICOLON 

	VOID <- 'void'
	PRIMITIVE <- 'char' / 'short' / 'int' / 'long' / 'float' / 'double'
	JUMP <- 'return' / 'goto' / 'break' / 'continue'
	SELECTION <- 'if' / 'else' / 'switch'
	ITERATION <- 'while' / 'do' / 'for'
	STORAGE <- 'auto' / 'register' / 'static' / 'const'
	LABEL <- 'case' / 'default'
	KEYWORD <- VOID / PRIMITIVE / JUMP / SELECTION / ITERATION / LABEL / STORAGE
	IDENTIFIER <- (! (KEYWORD %s) ) [_%w][_%w%d]*

	VAR_TYPE <- PRIMITIVE / IDENTIFIER
	RETURN_TYPE <- VOID / VAR_TYPE

	ENDING_SEMICOLON <- S ';' S
	UNUARY_OP <- [&-]
	BINARY_OP <- [<>*/+-] / '==' / '=' / '!=' / '>=' / '<='

	HEXCHAR <- [0-9a-fA-F]
	SINGLE_LINE_COMMENT <- ('//' / '#') [^%nl]* %nl
	MULTILINE_COMMENT <- '/*' ([^*] / ('*' !'/' ))* '*/'
	COMMENT <- SINGLE_LINE_COMMENT / MULTILINE_COMMENT
	S <- (%s / COMMENT)*
]], {
	hexadecimal_integer = function(str)
		return create_value('integer', tonumber(str:sub(3), 16))
	end,
	decimal_integer = function(str)
		return create_value('integer', tonumber(str))
	end,
	float = function(str)
		return create_value('float', tonumber(str))
	end,
	normal_char = function(char)
		return create_value('character', string.byte(char))
	end,
	escaped_char = function(char)
		return create_value('character', escaped_char_map[char])
	end,
	string = function(chars)
		return create_value('string', string.char(unpack(chars)))
	end,
	variable = function(t)
		return create_value('variable', t.name)
	end,
	escaped_char_map = escaped_char_map,
	string_byte = string.byte,
	parameter = function(t)
		t.quad.type = t.type
		return t.quad
	end,
})

local session = {}
session.__index = session

function session.new()
	return setmetatable({}, session)
end

function session:load(filename)
	local file = io.open(filename)
	local content = file:read '*a'
	file:close()

	self.definitions = grammar:match(content)
	
	return self.definitions
end

function session:show_definitions(format)
	format = format or 'yaml'
	if format == 'json' then
		local json = require 'dkjson'
		print(json.encode(self.definitions, {indent = true}))
	elseif format == 'yaml' then
		local yaml = require 'yaml'
		print(yaml.dump(self.definitions))
	end
end

local function map(list, func)
	local mapped = {}
	for _, elem in ipairs(list) do
		table.insert(mapped, func(elem))
	end
	return mapped
end

function session:dump()
	if self.definitions then
		local lines = map(self.definitions, function(definition) 
			if definition.definition == 'global' then
				local initializer = definition.initializer
				local space = ' '
				if definition.type:find('[*]$') then
					space = ''
				end
				if initializer then
					return ("%s%s%s = %s;"):format(definition.type, space, definition.name, initializer:tostring())
				else
					return ("%s%s%s;"):format(definition.type, space, definition.name)
				end
			end
		end)

		return table.concat(lines, "\n")
 	end
end

return {
	session = session
}