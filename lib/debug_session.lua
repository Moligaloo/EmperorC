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

local literal_integer_mt = {
	__index = {
		tostring = function(self)
			return ("%d"):format(self.value)
		end,
		size = function() return 4 end
	}
}

local literal_float_mt = {
	__index = {
		tostring = function(self)
			return ("%g"):format(self.value)
		end,
		size = function() return 4 end
	}
}

local literal_character_mt = {
	__index = {
		tostring = function(self)
			return ("'%s'"):format(readable_char(self.value, 'char'))
		end,
		size = function() return 1 end
	}
}

local string_mt = {
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

local grammar = re.compile([[
	definitions <- {| definition+ |}
	definition <- global_variable_definition
	global_variable_definition <- {|
		{type_specifier} %s* {IDENTIFIER} %s* {: static_initializer :}? %s* ';' %s*
	|} -> global_variable_definition
	type_specifier <- primitive_type (%s+ '*'+)?
	primitive_type <- 'int' / 'float' / 'char'
	static_initializer <- '=' %s* {: literal_value :}
	literal_value <- float / integer / character / string
	integer <- hexadecimal_integer / decimal_integer
	hexadecimal_integer <- ('0x' HEXCHAR+) -> literal_hexadecimal_integer
	decimal_integer <- (%d+) -> literal_decimal_integer
	float <- (%d+ '.' %d+) -> literal_float
	character <- "'" single_character "'"
	single_character <- ('\' { [abfnrtv'] } ) -> escaped_char / [^'] -> normal_char 
	string <- '"' {| char_in_string* |} -> string '"'
	char_in_string <- ('\' {[abfnrtv"]} ) -> escaped_char_map / [^"] -> string_byte

	IDENTIFIER <- [_%w][_%w%d]*
	HEXCHAR <- [0-9a-fA-F]
]], {
	global_variable_definition = function(captures)
		return {
			definition = 'global',
			type = captures[1],
			name = captures[2],
			initializer = captures[3]
		}
	end,
	literal_hexadecimal_integer = function(str)
		return setmetatable(
			{type = 'literal_integer', value = tonumber(str, 16)},
			literal_integer_mt
		)
	end,
	literal_decimal_integer = function(str)
		return setmetatable(
			{type = 'literal_integer', value = tonumber(str)}, 
			literal_integer_mt
		)
	end,
	literal_float = function(str)
		return setmetatable(
			{type = 'literal_float', value = tonumber(str)},
			literal_float_mt
		)
	end,
	normal_char = function(char)
		return setmetatable(
			{type = 'character', value = string.byte(char) },
			literal_character_mt
		)
	end,
	escaped_char = function(char)
		return setmetatable(
			{type = 'character', value = escaped_char_map[char] },
			literal_character_mt
		)
	end,
	string = function(chars)
		return setmetatable(
			{type = 'string', value = string.char(unpack(chars))},
			string_mt
		)
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