local emperor = require 'lib.emperor_c'

local session = emperor.session.new()
session:load 'c/hello.c'

session:show_definitions('json')


