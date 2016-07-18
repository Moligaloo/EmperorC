local debug_session = require 'lib.debug_session'

local session = debug_session.new()

session:load 'c/foo.c'
print(session:dump())

