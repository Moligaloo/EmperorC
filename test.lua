local emperor = require 'lib.emperor_c'

local json = require 'dkjson'

function io_read(filename)
	local fp = io.open(filename, 'r')
	local content = fp:read '*a'
	fp:close()
	return content
end

for _, file in ipairs{'hello', 'globals', 'while', 'expression', 'selection'} do
	it("Testing file " .. file, function() 
		local c_source = ("c/%s.c"):format(file)
		local json_source = io_read(("ast/%s.json"):format(file))

		assert.same(emperor.session.new():load(c_source), json.decode(json_source))
	end)
end
