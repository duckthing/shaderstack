--[[
Copyright (C) 2025 duckthing

This software is provided 'as-is', without any express or implied
warranty.  In no event will the authors be held liable for any damages
arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must not
   claim that you wrote the original software. If you use this software
   in a product, an acknowledgment in the product documentation would be
   appreciated but is not required.
2. Altered source versions must be plainly marked as such, and must not be
   misrepresented as being the original software.
3. This notice may not be removed or altered from any source distribution.
--]]

local ShaderStack = require "shaderstack"

-- Create an interesting shape
local function createCircle(points, radius)
	local arr = {}
	local factor = math.pi * 2 / points
	for i = 1, points do
		local angle = factor * i
		arr[#arr+1], arr[#arr+2] =
			math.cos(angle) * radius,
			math.sin(angle) * radius
	end
	return arr
end
local radius = 200
local points = createCircle(16, radius)

-- Create the shaders and put them in a table
-- (You don't have to do this; it's just for readability)
local shaderSources = {
blink = [[
uniform float strength;

#ifdef PIXEL
vec4 effect(vec4 color, Image tex, vec2 texture_coords, vec2 screen_coords)
{
	return vec4(color.rgb * strength, color.a);
}
#endif
]],

grayscale = [[
const float factor = 0.333333;
float getAverage(vec3 color) {
	return (color.r + color.g + color.b) * factor;
}

#ifdef PIXEL
vec4 effect(vec4 color, Image tex, vec2 texture_coords, vec2 screen_coords)
{
	float avg = getAverage(color.rgb);
	return vec4(avg, avg, avg, color.a);
}
#endif
]],

wobble = [[
uniform float TIME;
uniform float strength;

#ifdef VERTEX
vec4 position(mat4 transform_projection, vec4 vertex_position)
{
	return vertex_position + vec4(0., sin(vertex_position.x + TIME) * strength, 0., 0.);
}
#endif
]],

rainbow = [[
// I know it's ugly

uniform float TIME;

#ifdef PIXEL
vec4 effect(vec4 color, Image tex, vec2 texture_coords, vec2 screen_coords)
{
	vec3 result = vec3(
		sin(TIME + screen_coords.x * 0.05),
		cos(TIME + screen_coords.x * 0.05),
		cos(TIME + screen_coords.y * 0.05)
	);
	return vec4(color.rgb * result, color.a);
}
#endif
]]
}

function love.load()
	love.graphics.setLineWidth(64)

	for tag, source in pairs(shaderSources) do
		ShaderStack.addTag(tag, source)
	end

	-- "grayscale" is a bit annoying to type;
	-- add an alias for it!
	ShaderStack.addAlias("grayscale", "gray")

	ShaderStack.send("wobble", "strength", 20)
end

local stackStr = "Stack:"
function love.draw()
	love.graphics.setShader()
	love.graphics.print(stackStr)

	local windowW, windowH = love.graphics.getDimensions()
	ShaderStack.use()
	love.graphics.translate(windowW * 0.5, windowH * 0.5)
	love.graphics.polygon("line", points)
end

function love.update()
	-- Send a value to a tagged Shader;
	-- all variants will get updated automatically!
	local time = love.timer.getTime()
	ShaderStack.send("blink", "strength", math.sin(time * 2) * 0.5 + 1)
	ShaderStack.send("wobble", "TIME", time)
	ShaderStack.send("rainbow", "TIME", time)
end

function love.keypressed(key)
	local method = "push"
	if love.keyboard.isDown("lshift") then method = "pop" end
	if key == "1" then
		ShaderStack[method]("blink", true)
	elseif key == "2" then
		-- Lets use the alias!
		ShaderStack[method]("gray", true)
	elseif key == "3" then
		ShaderStack[method]("wobble", true)
	elseif key == "4" then
		ShaderStack[method]("rainbow", true)
	elseif key == "5" then
		ShaderStack.reset()
	end
	stackStr = "Stack: "..table.concat(ShaderStack.getStack(), ", ")
end
