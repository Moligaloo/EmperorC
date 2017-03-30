local re = require 're'
local json = require 'dkjson'

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

local function map(list, func)
	local mapped = {}
	for _, elem in ipairs(list) do
		local result = func(elem)
		if result then
			table.insert(mapped, result)
		end
	end
	return mapped
end

local fill_template

local template_func = function(template, sep)
	return function(self) return fill_template(template, self, sep) end
end

local ast_to_string_table = {
	definition = {
		global = template_func('${modifiers }${type} ${quads};', {modifiers = ' ', quads = ', '}),
		['function'] = template_func('${return_type} ${name}(${parameters}){\n${body}\n}', {parameters = ', ', body = '\n'})
	}
}

local function ast_to_string(ast, node)
	node = node or ast_to_string_table
	for key, subnode in pairs(node) do
		local astkey = ast[key]
		if astkey then
			local grandnode = subnode[astkey]
			local grandnode_type = type(grandnode)
			if grandnode_type == 'string' then
				return fill_template(grandnode, ast)
			elseif grandnode_type == 'function' then
				return grandnode(ast)
			elseif grandnode_type == 'table' then
				return ast_to_string(ast, grandnode)
			else
				error(("Unknown node type: %s"):format(grandnode_type))
			end
		else
			error(("ast %s can not cast to string"):format(json.encode(ast)))
		end
	end
end

local function object_tostring(object)
	if type(object) == 'table' then
		if object.tostring then
			return object:tostring()
		else
			return ast_to_string(object)
		end
	else
		return tostring(object)
	end
end

function fill_template(template, t, sep)
	local result = template:gsub("%${([^%w_]*)([%w_]+)([^%w_}]*)}", function(prefix, key, suffix)
			local result = t[key]
			if result == nil then
				return ''
			elseif type(result) == 'table' then
				if getmetatable(result) == nil then
					result = table.concat(map(result, function(e) return object_tostring(e) end), sep[key])
				else
					result = object_tostring(result)
				end
			end

			return ("%s%s%s"):format(prefix, result, suffix)
		end)

	return result
end

local tostring_metatable = function(tostring_func)
	return { __index = { tostring = tostring_func }}
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

local statement_tostring_table = {
	expression = '${expression};',
	['return'] = 'return ${value};',
	vardef = template_func('${modifiers}${type} ${quads};', {modifiers = ' ', quads = ', '}),
	['while'] = 'while(${condition})\n${body}',
	['for'] = 'for(${init} ${condition}; ${next})\n${body}',
	['if'] = function(self)
		if self['else'] then
			return fill_template('if(${condition})${body}else ${else}', self)
		else
			return fill_template('if(${condition})${body}', self)
		end
	end,
	compound = template_func('{\n${statements}\n}', {statements = '\n'}),
}

local expression_tostring_table = {
	post_increment = '${ref}++',
	post_decrement = '${ref}--',
	greater = '${left}>${right}',
	less = '${left}<${right}',
	less_equal = '${left} <= ${right}',
	greater_equal = '${left} >= ${right}',
	call = template_func('${function}(${arguments})', {arguments = ', '}),
	pre_increment = '++${expression}',
	pre_decrement = '--${expression}',
	negate = '-${expression}',
	['not'] = '!${expression}',
	cast = '(${cast})${expression}',
	deref = '*${expression}',
	addr = '&${expression}',
	sizeof = function(self) 
		return fill_template(self.vartype and 'sizeof(${vartype})' or 'sizeof(${expression})', self)
	end,
	multiply = '${left}*${right}',
	modular = '${left}%${right}',
	subtract = '${left}-${right}',
	add = '${left}+${right}',
	left_shift = '${left} << ${right}',
	right_shift = '${left} >> ${right}',
	greater_equal = '${left} >= ${right}',
	equal = '${left} == ${right}',
	not_equal = '${left} != ${right}',
	bitwise_and = '${left} & ${right}',
	bitwise_or = '${left} | ${right}',
	bitwise_xor = '${left} ^ ${right}',
	logic_and = '${left} && ${right}',
	logic_or = '${left} || ${right}',
	ternary = '${condition} ? ${yes} : ${no}',
	assign = '${left} ${extra}= ${right}',
	subscript = '${ref}[${subscript}]',
	arrow = '${ref}->${member}',
	dot = '${ref}.${member}',
}

local tostring_func_from_table = function(t, field)
	return function(self)
		local key = self[field]
		local value = t[key]
		if type(value) == 'string' then
			return fill_template(value, self)
		elseif type(value) == 'function' then
			return value(self)
		else
			error("no tostring for object:" .. json.encode(self))
		end
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
						elseif byte == 0x20 then
							return ' '
						else
							return ("\\x%02X"):format(byte)
						end
					end
				end))
			end,
			size = function() return #self.value + 1 end
		}
	},
	vardef_quad = tostring_metatable(template_func '${stars}${name}${[array_count]}${ = initializer}'),
	parameter = tostring_metatable(template_func '${type }${stars}${name}'),
	statement = tostring_metatable(tostring_func_from_table(statement_tostring_table, 'statement')),
	variable = tostring_metatable(function(self) return self.value end),
	expression = tostring_metatable(tostring_func_from_table(expression_tostring_table, 'type')),
}

local function create_value(type, value)
	return setmetatable({type = type, value = value}, metatables[type])
end

local function create_mt_setter(metatable_name)
	local mt = metatables[metatable_name]
	return function(t) return setmetatable(t, mt) end
end

-- grammar rule name convention:
-- 1. terminal symbols use all uppercased name: e.g. SEMICOLON
-- 2. non terminal symbols use all lowercased name, word separated by _, e.g. global_variable_definition

local grammar = re.compile([[
	definitions <- {| (S definition S)+ |}
	definition <- (function_definition / global_variable_definition)
	global_variable_definition <- {| {:definition: '' -> 'global' :} <vardef> |} 

	-- literal value
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
	function_head <- {:return_type:RETURN_TYPE:} S {:name:IDENTIFIER:} S '(' S {:parameters:parameters:}? S ')'
	function_body <- compound_statement -> statements_from_compound

	expression <- p14_expression -> expression
	p0_expression <- 
		literal_value 
		/ variable 
		/ PAREN_L expression PAREN_R
	p1_expression <- 
		{| {:primary:p0_expression:} {:postfixes: {| p1_postfix+ |} :}? |} -> p1_tree
	p1_postfix <- 
		{| {:postfix: '' -> 'subscript' :} BRACKET_L {:subscript:expression:} BRACKET_R |}
		/ {| {:postfix: '' -> 'dot' :} S '.' S {:member:IDENTIFIER:} |}
		/ {| {:postfix: '' -> 'arrow':} S '->' S {:member:IDENTIFIER:} |}
		/ {| {:postfix: '' -> 'call':} PAREN_L {:arguments:arguments:}? PAREN_R |} 
		/ {| {:postfix: '++' -> 'post_increment' :} |}
		/ {| {:postfix: '--' -> 'post_decrement' :} |}
	p2_expression <-
		p1_expression
		/ {| {:type: '++' -> 'pre_increment' :} S {:expression:p2_expression:} |}
		/ {| {:type: '--' -> 'pre_decrement' :} S {:expression:p2_expression:} |}
		/ {| {:type: '-' -> 'negate' :} S {:expression:p2_expression:} |} -> negate_number
		/ {| {:type: '!' -> 'not' :} S {:expression:p2_expression:} |}
		/ {| {:type: '~' -> 'complement' :} S {:expression:p2_expression:} |}
		/ {| {:type: '' -> 'cast' :} PAREN_L {:cast:VAR_TYPE:} PAREN_R {:expression:p2_expression:} |}
		/ {| {:type: '*' -> 'deref' :} S {:expression:p2_expression:} |}
		/ {| {:type: '&' -> 'addr' :} S {:expression:p2_expression:} |}
		/ {| {:type: 'sizeof' :} S {:expression:expression:} |}
		/ {| {:type: 'sizeof' :} S {:vartype:VAR_TYPE:} |}

	p3_expression <- {| {:left:p2_expression:} {:suffixes: {| p3_suffix+ |} :}? |} -> common_tree
	p3_suffix <- {| S {:type: p3_operator :} S {:right:p2_expression:} |}
	p3_operator <- '*' -> 'multiply' / '%' -> 'modular'

	p4_expression <- {| {:left:p3_expression:} {:suffixes: {| p4_suffix+ |} :}? |} -> common_tree
	p4_suffix <- {| S {:type: p4_operator :} S {:right:p3_expression:} |}
	p4_operator <- '+' -> 'add' / '-' -> 'subtract'

	p5_expression <- {| {:left:p4_expression:} {:suffixes: {| p5_suffix+ |} :}? |} -> common_tree
	p5_suffix <- {| S {:type: p5_operator :} S {:right:p4_expression:} |}
	p5_operator <- '<<' -> 'left_shift' / '>>' -> 'right_shift'

	p6_expression <- {| {:left:p5_expression:} {:suffixes: {| p6_suffix+ |} :}? |} -> common_tree
	p6_suffix <- {| S {:type: p6_operator :} S {:right:p5_expression:} |}
	p6_operator <- 
		'<=' -> 'less_equal' 
		/ '<' -> 'less'
		/ '>=' -> 'greater_equal'
		/ '>' -> 'greater'

	p7_expression <- {| {:left:p6_expression:} {:suffixes: {| p7_suffix+ |} :}? |} -> common_tree
	p7_suffix <- {| S {:type: p7_operator :} S {:right:p6_expression:} |}
	p7_operator <- '==' -> 'equal' / '!=' -> 'not_equal'

	p8_expression <- {| {:left:p7_expression:} {:suffixes: {| p8_suffix+ |} :}? |} -> common_tree
	p8_suffix <- {| S {:type: ('&' (![&]) ) -> 'bitwise_and' :} S {:right:p7_expression:} |}

	p9_expression <- {| {:left:p8_expression:} {:suffixes: {| p9_suffix+ |} :}? |} -> common_tree
	p9_suffix <- {| S {:type: '^' -> 'bitwise_xor' :} S {:right:p8_expression:} |}

	p10_expression <- {| {:left:p9_expression:} {:suffixes: {| p10_suffix+ |} :}? |} -> common_tree
	p10_suffix <- {| S {:type: ('|' (![|])) -> 'bitwise_or' :} S {:right:p9_expression:} |}

	p11_expression <- {| {:left:p10_expression:} {:suffixes: {| p11_suffix+ |} :}? |} -> common_tree
	p11_suffix <- {| S {:type: '&&' -> 'logic_and' :} S {:right:p10_expression:} |}

	p12_expression <- {| {:left:p11_expression:} {:suffixes: {| p12_suffix+ |} :}? |} -> common_tree
	p12_suffix <- {| S {:type: '||' -> 'logic_or' :} S {:right:p11_expression:} |}

	p13_expression <-
		{| {:type:''->'ternary':} {:condition:p12_expression:} S '?' S {:yes:p12_expression:} S ':' S {:no:p12_expression:} |}
		/ p12_expression

	p14_expression <-
		{| {:left:p13_expression:} S {:extra:EXTRA_ASSIGN:}? {:type:'='->'assign':} S {:right:p14_expression:} |}
		/ p13_expression

	variable <- {| {:name: IDENTIFIER :} |} -> variable
	arguments <- {| argument (S ',' S argument)* |}
	argument <- expression

	-- statements
	statement <- 
		(compound_statement 
		/ jump_statement 
		/ vardef_statement 
		/ expression_statement 
		/ iteration_statement
		/ selection_statement) -> statement
	compound_statement <- {| {:statement: '' -> 'compound' :} BRACE_L {:statements:statements:} BRACE_R |} 
	expression_statement <- {| {:statement: '' -> 'expression' :} {:expression: expression :} SEMICOLON |}
	iteration_statement <- {| iteration_while / iteration_for |}
	statements <- {| statement* |}
	selection_statement <- selection_if
	selection_if <- 
		{| {:statement:'if':} PAREN_L {:condition:expression:} PAREN_R {:body:statement:} {:else:selection_else:}? |}
	selection_else <-
		'else' S {: statement :}

	iteration_while <- {:statement: 'while' :} PAREN_L {:condition:expression:} PAREN_R {:body:statement:} 
	iteration_for <- 
		{:statement: 'for' :} PAREN_L {:init:statement:} {:condition:expression:} S ';' S {:next:expression:} PAREN_R {:body:statement:}
	
	jump_statement <- {| (jump_goto / jump_continue / jump_break / jump_return) SEMICOLON |}
	jump_goto <- {:statement: 'goto' :} S {:label: IDENTIFIER :}
	jump_continue <- {:statement: 'continue' :}
	jump_break <- {:statement: 'break' :}
	jump_return <- {:statement: 'return' :} S {:value: expression :}?

	vardef_statement <- {| {:statement: '' -> 'vardef' :} <vardef> |} 
	parameter <- {| {:type: VAR_TYPE :} S <vardef_stars>? S {:name:IDENTIFIER:} |} -> parameter
	parameters <- {| parameter (S ',' S parameter)* |}

	vardef_stars <- {:stars: [*]+ :}
	vardef_name <- {:name: IDENTIFIER :}
	vardef_bracket <- {:array_count: '[' integer ']' :}
	vardef_initializer <- '=' S {:initializer: expression :}
	vardef_quad <- {| <vardef_stars>? S <vardef_name> S <vardef_bracket>? S <vardef_initializer>? |} -> vardef_quad
	vardef_quads <- {| vardef_quad (S ',' S vardef_quad)* |}
	vardef_modifier <- S {STORAGE} S
	vardef_modifiers <- {| vardef_modifier+ |}
	vardef <-  S {:modifiers:vardef_modifiers:}? S {:type: VAR_TYPE :} S {:quads: vardef_quads :} SEMICOLON 

	VOID <- 'void'
	PRIMITIVE <- 'char' / 'short' / 'int' / 'long' / 'float' / 'double'
	JUMP <- 'return' / 'goto' / 'break' / 'continue'
	SELECTION <- 'if' / 'else' / 'switch'
	ITERATION <- 'while' / 'do' / 'for'
	STORAGE <- 'auto' / 'register' / 'static' / 'const'
	LABEL <- 'case' / 'default'
	OPERATOR <- 'sizeof'
	KEYWORD <- VOID / PRIMITIVE / JUMP / SELECTION / ITERATION / LABEL / STORAGE / OPERATOR
	IDENTIFIER <- (! (KEYWORD [^_%w%d] ) ) [_%w][_%w%d]*

	VAR_TYPE <- PRIMITIVE / IDENTIFIER
	RETURN_TYPE <- VOID / VAR_TYPE

	SEMICOLON <- S ';' S
	HEXCHAR <- [0-9a-fA-F]
	SINGLE_LINE_COMMENT <- ('//' / '#') [^%nl]* %nl
	MULTILINE_COMMENT <- '/*' ([^*] / ('*' !'/' ))* '*/'
	COMMENT <- SINGLE_LINE_COMMENT / MULTILINE_COMMENT
	S <- (%s / COMMENT)*
	PAREN_L <- S '(' S
	PAREN_R <- S ')' S
	BRACE_L <- S '{' S
	BRACE_R <- S '}' S
	BRACKET_L <- S '[' S
	BRACKET_R <- S ']' S
	EXTRA_ASSIGN <-
		[-+*/%&^|] / '<<' / '>>'
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
	statements_from_compound = function(compound_statement)
		return compound_statement.statements
	end,
	p1_tree = function(p1)
		local tree = p1.primary
		local postfixes = p1.postfixes
		if postfixes then
			for _, postfix in ipairs(postfixes) do
				local type = postfix.postfix
				if type == 'call' then
					tree = {
						type = 'call',
						['function'] = tree,
						arguments = postfix.arguments
					}
					setmetatable(tree, metatables.expression)
				elseif type == 'dot' or type == 'arrow' or type == 'post_increment' or type == 'post_decrement' then
					tree = {
						type = type,
						ref = tree,
						member = postfix.member
					}
					setmetatable(tree, metatables.expression)
				elseif type == 'subscript' then
					tree = {
						type = 'subscript',
						ref = tree,
						subscript = postfix.subscript
					}
					setmetatable(tree, metatables.expression)
				end
			end
		end

		return tree
	end,
	negate_number = function(p2)
		local p2_expression = p2.expression
		if p2_expression.type == 'float' or p2_expression.type == 'integer' then
			p2_expression.value = -p2_expression.value
			return p2_expression
		end
		return p2
	end,
	common_tree = function(t)
		if t.suffixes == nil then
			return t.left
		end

		local left = t.left
		local expression
		for _, suffix in ipairs(t.suffixes) do
			expression = {
				left = left,
				type = suffix.type,
				right = suffix.right
			}
			setmetatable(expression, metatables.expression)
			left = expression
		end
		return expression
	end,
	vardef_quad = create_mt_setter('vardef_quad'),
	parameter = create_mt_setter('parameter'),
	statement = create_mt_setter('statement'),
	expression = function(t) return getmetatable(t) and t or setmetatable(t, metatables.expression) end,
})

local function read_file(filename)
	local file = io.open(filename)
	local content = file:read '*a'
	file:close()

	return content
end

local function write_file(filename, content)
	local file = io.open(filename, 'w')
	file:write(content)
	file:close()
end

local session = {}
session.__index = session

function session.new()
	return setmetatable({}, session)
end

function session:load_c(filename)
	self.definitions = grammar:match(read_file(filename))
end

function session:to_ast(filename)
	write_file(filename, json.encode(self.definitions, {indent = true}))
end

function session:show_definitions(format)
	format = format or 'yaml'
	if format == 'json' then
		print(json.encode(self.definitions, {indent = true}))
	elseif format == 'yaml' then
		local yaml = require 'yaml'
		print(yaml.dump(self.definitions))
	end
end

function session:decompile()
	return fill_template("${definitions}", self, {definitions = "\n"})
end

return {
	session = session
}