require 're'

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
	definitions <- {| (definition S)+ |}
	definition <- function_definition / global_variable_definition
	global_variable_definition <- {|
		{type_specifier} S {IDENTIFIER} S {: static_initializer :}? ENDING_SEMICOLON
	|} -> global_variable_definition
	type_specifier <- PRIMITIVE (S '*'+)?
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
	function_head <- {:return_type:return_type:} S {:name:IDENTIFIER:} '(' S {:parameters:parameters:} S ')'
	return_type <- PRIMITIVE / 'void'
	function_body <- {| S '{' S statement* S '}' S |}
	statement <- return_statement / assignment_statement / expression_statement
	expression <- call_expression / unary_expression / binary_expression / term
	unary_expression <- {| {:op: UNUARY_OP :} S {:A: term :} |}
	binary_expression <- {| {:A: term :} S {:op: BINARY_OP :} S {:B: term :} |}
	call_expression <- {| {:function_name:IDENTIFIER:} S {:arguments: '(' S {: arguments :} S ')' :} |}
	expression_statement <- {| {:statement: '' -> 'expression' :} {:expression: expression :} ENDING_SEMICOLON |}
	assignment_statement <- IDENTIFIER S '=' S expression ENDING_SEMICOLON
	return_statement <- {| {:statement: 'return' :} S {:value: expression :} ENDING_SEMICOLON |} 
	term <- literal_value / variable
	variable <- {| {:name: IDENTIFIER :} |} -> variable
	arguments <- {| argument (S ',' S argument)* |} / ''
	argument <- expression
	parameter <- {| {:type: type_specifier :} S {:name: IDENTIFIER :} |}
	parameters <- {| parameter (S ',' S parameter)* |} / '' 
	
	ENDING_SEMICOLON <- S ';' S
	UNUARY_OP <- '-'
	BINARY_OP <- [<>*/+-] / '==' / '!=' / '>=' / '<='
	PRIMITIVE <- 'int' / 'float' / 'char'
	IDENTIFIER <- [_%w][_%w%d]*
	HEXCHAR <- [0-9a-fA-F]
	S <- (%s)*
]], {
	global_variable_definition = function(captures)
		return {
			definition = 'global',
			type = captures[1],
			name = captures[2],
			initializer = captures[3]
		}
	end,
	hexadecimal_integer = function(str)
		return create_value('integer', tonumber(str, 16))
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
	string_byte = string.byte
})

local debug_session = {}
debug_session.__index = debug_session

function debug_session.new()
	local session = {}
	setmetatable(session, debug_session)
	return session
end

function debug_session:load(filename)
	local file = io.open(filename)
	local content = file:read '*a'
	file:close()

	self.definitions = grammar:match(content)
	
	return self.definitions
end

local function map(list, func)
	local mapped = {}
	for _, elem in ipairs(list) do
		table.insert(mapped, func(elem))
	end
	return mapped
end

function debug_session:dump()
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

return debug_session