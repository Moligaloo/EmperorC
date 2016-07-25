local emperor = require 'lib.emperor_c'

local session = emperor.session.new()
local yaml = require 'yaml'

print(yaml.dump(session:load 'c/hello.c'))

