local emperor = require 'lib.emperor_c'

local filename = arg[1] or "c/hello.c"

local session = emperor.session.new()
session:load(filename)

session:show_definitions('json')


