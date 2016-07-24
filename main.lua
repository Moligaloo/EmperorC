local debug_session = require 'lib.debug_session'

local session = debug_session.new()
local yaml = require 'yaml'

print(yaml.dump(session:load 'c/hello.c'))

