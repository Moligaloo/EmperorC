local emperor = require 'lib.emperor_c'

local yaml = require 'yaml'

for _, file in ipairs{'hello', 'globals'} do
	it("Testing file " .. file, function() 
		local c_source = ("c/%s.c"):format(file)
		local yaml_source = ("yaml/%s.yaml"):format(file)

		assert.same(emperor.session.new():load(c_source), yaml.loadpath(yaml_source))
	end)
end
