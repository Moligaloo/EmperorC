package.path = package.path .. ";./?/init.lua"

local emperor = require 'emperor_c'
local json = require '3rd.dkjson'

function io_read(filename)
	local fp = io.open(filename, 'r')
	local content = fp:read '*a'
	fp:close()
	return content
end

local filenames = {'hello', 'globals', 'while', 'expression', 'selection'}

for _, file in ipairs(filenames) do
	it("Testing file compiling " .. file, function() 
		local c_source = ("test/c/%s.c"):format(file)
		local json_source = io_read(("test/ast/%s.json"):format(file))
		local session = emperor.session.new()
		session:load_c(c_source)

		assert.same(session.definitions, json.decode(json_source))
	end)
end

for _, file in ipairs(filenames) do
	it("Testing file decompiling " .. file, function()
		local session = emperor.session.new()
		session:load_ast(("test/ast/%s.json"):format(file))
		session:to_c()
	end)
end
