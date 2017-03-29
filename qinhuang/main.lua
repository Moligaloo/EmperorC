package.path = package.path .. ';./3rd/?.lua;./?/init.lua'
package.cpath = package.cpath .. ';./3rd/?.so'

local emperor = require 'emperor_c'

local g = love.graphics

function love.filedropped(file)
	
end

function love.draw()

end
