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

---@class ShaderStack
local ShaderStack = {}

---@type boolean # Should the Shader be set immediately after changes to the stack?
local updateOnChange = false

---Should changing the stack update the shader?
---Setting this to `true` can create a large amount of intermediary shaders that are not used.
---@param newVal boolean
function ShaderStack.setAutomaticUpdates(newVal) updateOnChange = newVal end

---@class ShaderStack.Parsed
---@field finalSource string
---@field swapMap {[string]: string} # A map of member names to their new names
---@field sentValues {[string]: any} # A map of members and their values; used when creating a new Shader variant
---@field related love.Shader[] # Shaders that were created with this tag

---Shaders tags to their original source code
---@type {[string]: string}
local tags = {}

---Shader tags to their parsed contents
---@type {[string]: ShaderStack.Parsed}
local parsedTags = {}

---An alias maps to a tag name, in case the original tag is difficult to write
---@type {[string]: string}
local aliases = {}

---A stack of tags
---@type string[]
local tagStack = {}

---A map of shader tags to other shader tags or the resulting shader
---An index of `1` returns the resulting shader
local shaderMap = {
	[1] = love.graphics.newShader([[
#pragma language glsl3
#ifdef VERTEX
vec4 position(mat4 transform_projection, vec4 vertex_position)
{
	return transform_projection * vertex_position;
}
#endif

#ifdef PIXEL
vec4 effect(vec4 color, Image tex, vec2 texture_coords, vec2 screen_coords)
{
	return color * Texel(tex, texture_coords);
}
#endif
]])
}

local reservedSymbols = {
	-- Current reserved symbols (GLSL3)
	attribute = true, const = true, uniform = true, varying = true,
	layout = true,
	centroid = true, flat = true, smooth = true, noperspective = true,
	["break"] = true, continue = true, ["do"] = true, ["for"] = true, ["while"] = true, switch = true, case = true, default = true,
	["if"] = true, ["else"] = true,
	["in"] = true, out = true, inout = true,
	float = true, int = true, void = true, bool = true, ["true"] = true, ["false"] = true,
	invariant = true,
	discard = true, ["return"] = true,
	mat2 = true, mat3 = true, mat4 = true,
	mat2x2 = true, mat2x3 = true, mat2x4 = true,
	mat3x2 = true, mat3x3 = true, mat3x4 = true,
	mat4x2 = true, mat4x3 = true, mat4x4 = true,
	vec2 = true, vec3 = true, vec4 = true, ivec2 = true, ivec3 = true, ivec4 = true, bvec2 = true, bvec3 = true, bvec4 = true,
	uint = true, uvec2 = true, uvec3 = true, uvec4 = true,
	lowp = true, mediump = true, highp = true, precision = true,
	sampler1D = true, sampler2D = true, sampler3D = true, samplerCube = true,
	sampler1DShadow = true, sampler2DShadow = true, samplerCubeShadow = true,
	sampler1DArray = true, sampler2DArray = true,
	sampler1DArrayShadow = true, sampler2DArrayShadow = true,
	isampler1D = true, isampler2D = true, isampler3D = true, isamplerCube = true,
	isampler1DArray = true, isampler2DArray = true,
	usampler1D = true, usampler2D = true, usampler3D = true, usamplerCube = true,
	usampler1DArray = true, usampler2DArray = true,
	sampler2DRect = true, sampler2DRectShadow = true, isampler2DRect = true, usampler2DRect = true,
	samplerBuffer = true, isamplerBuffer = true, usamplerBuffer = true,
	sampler2DMS = true, isampler2DMS = true, usampler2DMS = true,
	sampler2DMSArray = true, isampler2DMSArray = true, usampler2DMSArray = true,
	struct = true,

	-- Reserved for future use (GLSL3)
	common = true, partition = true, active = true,
	asm = true,
	class = true, union = true, enum = true, typedef = true, template = true, this = true, packed = true,
	goto = true,
	inline = true, noinline = true, volatile = true, public = true, static = true, extern = true, external = true, interface = true,
	long = true, short = true, double = true, half = true, fixed = true, unsigned = true, superp = true,
	input = true, output = true,
	hvec2 = true, hvec3 = true, hvec4 = true, dvec2 = true, dvec3 = true, dvec4 = true, fvec2 = true, fvec3 = true, fvec4 = true,
	sampler3DRect = true,
	filter = true,
	image1D = true, image2D = true, image3D = true, imageCube = true,
	iimage1D = true, iimage2D = true, iimage3D = true, iimageCube = true,
	uimage1D = true, uimage2D = true, uimage3D = true, uimageCube = true,
	image1DArray = true, image2DArray = true,
	iimage1DArray = true, iimage2DArray = true, uimage1DArray = true, uimage2DArray = true,
	image1DShadow = true, image2DShadow = true,
	image1DArrayShadow = true, image2DArrayShadow = true,
	imageBuffer = true, iimageBuffer = true, uimageBuffer = true,
	sizeof = true, cast = true,
	namespace = true, using = true,
	row_major = true,

	-- Reserved by Love2D
	Image = true, ArrayImage = true, CubeImage = true, VolumeImage = true, Texel = true, number = true,
	gammaCorrectColor = true, gammaCorrectColorPrecise = true, unGammaCorrectColor = true, unGammaCorrectColorPrecise = true,
}

---Adds a tag that can be used in `:push()` and `:pop()`
---@param tag string
---@param contents string
---@param alias string? # Optional alias; refers to the original tag
function ShaderStack.addTag(tag, contents, alias)
	if type(contents) ~= "string" then
		error(("Parameter #2 needs the shader source in string form"))
	end

	if alias then ShaderStack.addAlias(tag, alias) end

	if tags[tag] then
		-- Error on duplicates
		error(("Tag '%s' already exists"):format(tag))
	end

	tags[tag] = contents
end

---Removes a tag (and by default, all aliases)
---@param tag string
function ShaderStack.removeTag(tag, includeAliases)
	if includeAliases ~= false then
		-- Removes aliases, unless `includeAliases` is false
		ShaderStack.removeAllAliasesForTag(tag)
	end

	-- TODO: When multiple of the same shader is allowed, improve this
	if ShaderStack.pop(tag) then
		ShaderStack.use()
	end

	-- Remove parsed data
	local parsed = parsedTags[tag]
	parsedTags[tag] = nil
	tags[tag] = nil

	-- Release all shaders created with this tag
	local related = parsed.related
	if #related > 0 then
		-- Shaders were created with this tag, remove all of them
		local removeSimilar
		function removeSimilar(t)
			if t[tag] then
				t[tag] = nil
			end

			for _, v in pairs(t) do
				if type(v) == "table" then
					removeSimilar(v)
				end
			end
		end

		removeSimilar(shaderMap)
	end
end

---Returns `true` if the tag exists
---@param tag string
---@return boolean exists
function ShaderStack.hasTag(tag)
	return tags[tag] ~= nil
end

---Adds an alias for a tag; multiple aliases can exist for one tag.
---Aliases refer to a tag, which can be used if the original tag is difficult to write or prone to changing
---@param tag string
---@param alias string
function ShaderStack.addAlias(tag, alias)
	if tag[alias] then
		-- Error on an alias equivalent to a tag
		error(("Alias '%s' (for '%s') cannot be mapped to an existing tag of the same name"):format(alias, tag, aliases[alias]))
	elseif aliases[alias] then
		-- Error on duplicates
		error(("Alias '%s' (for '%s') already exists as alias '%s'"):format(alias, tag, aliases[alias]))
	end
	aliases[alias] = tag
end

---Removes an alias
---@param alias string
function ShaderStack.removeAlias(alias)
	aliases[alias] = nil
end

---Removes all aliases that map to a certain tag
---@param tag string
function ShaderStack.removeAllAliasesForTag(tag)
	for k, v in pairs(aliases) do
		if v == tag then aliases[k] = nil end
	end
end

---Returns `true` as the first value if the parameters maps to a tag.
---Returns the tag the alias is mapped to as the second return value, if its an alias at all.
---@param alias string
---@return boolean resultsInTag
---@return string? mappedTag
function ShaderStack.hasAlias(alias)
	local resultingTag = aliases[alias]
	return (tags[alias] or resultingTag) ~= nil,
		resultingTag
end

---Sets the combination result of the stack `s` to `shader`.
---Useful if you combined something manually.
---@param s string[]? # Override stack
---@param shader love.Shader
function ShaderStack.setResultShader(s, shader)
	local stack = s or tagStack

	local map = shaderMap
	for i = 1, #stack do
		local tag = stack[i]
		local parsed = parsedTags[tag]

		if parsed then
			-- Add the shader as related (so we can send values to it)
			local related = parsed.related
			related[#related+1] = shader
		end

		local nextMap = map[tag]
		if not nextMap then
			nextMap = {}
			map[tag] = nextMap
		end
		map = nextMap
	end
	map[1] = shader
end

---Gets an existing combination result of the stack `s`, but will not create a new Shader
---@param s string[]? # Override stack
function ShaderStack.getResultShader(s)
	local stack = s or tagStack

	local map = shaderMap
	for i = 1, #stack do
		local tag = stack[i]
		local nextMap = map[tag]
		if not nextMap then
			return nil
		end
		map = nextMap
	end
	return map[1]
end

---Returns `true` if the tag is pushed
---@param tag string
---@return boolean pushed
function ShaderStack.isTagPushed(tag)
	if not tags[tag] then
		tag = aliases[tag]
		if not tags[tag] then
			return false
		end
	end
	local stack = tagStack
	for i = 1, #stack do
		if stack[i] == tag then
			return true
		end
	end
	return false
end

---Pushes a Shader tag to the stack
---@param tag string
---@param setImmediately boolean? # Sets the Shader after pushing
function ShaderStack.push(tag, setImmediately)
	if setImmediately == nil then setImmediately = updateOnChange end
	if not tags[tag] then
		tag = aliases[tag]
		if not tags[tag] then
			print(("[ShaderStack.push] Attempt to push non-existant tag '%s'; skipping"):format(tag))
			return
		end
	end

	local stack = tagStack
	for i = 1, #stack do
		if stack[i] == tag then
			print(("[ShaderStack.push] Attempted to push duplicate tag '%s'; skipping"):format(tag))
			return
		end
	end
	stack[#stack+1] = tag

	if setImmediately then ShaderStack.use() end
end

---Pops the top-most Shader (that matches the tag, if provided), and returns the tag that was popped
---@param tag string?
---@param setImmediately boolean? # Sets the Shader after pushing
---@return string? popped
function ShaderStack.pop(tag, setImmediately)
	if setImmediately == nil then setImmediately = updateOnChange end
	local popped
	if not tag then
		popped = tagStack[#tagStack]
		tagStack[#tagStack] = nil
	else
		if not tags[tag] then
			tag = aliases[tag]
			if not tags[tag] then
				print(("[ShaderStack.pop] Attempt to pop non-existant tag '%s'; skipping"):format(tag))
				return
			end
		end

		for i = #tagStack, 1, -1 do
			if tagStack[i] == tag then
				popped = table.remove(tagStack, i)
				break
			end
		end
	end

	if popped and setImmediately then ShaderStack.use() end
	return popped
end

---Returns `true` if `point` is within a comment
---@param set integer[]
---@param point integer
---@return boolean commented
local function isCommented(set, point)
	if #set == 0 then return false end
	for i = #set - 1, #set, 2 do
		local first, last = set[i], set[i+1]

		if point >= first and point < last then
			return true
		end
	end
	return false
end

---Returns the stack of shader tags
---@return string[] stack
function ShaderStack.getStack()
	return tagStack
end

---Gets the parsed contents of a Shader with a tag
---@param tag string
---@return ShaderStack.Parsed?
function ShaderStack.getParsed(tag)
	tag = (tags[tag] and tag) or aliases[tag]
	if not tag then return nil end
	do
		local parsed = parsedTags[tag]
		if parsed then return parsed end
	end

	local contents = tags[tag]
	if not contents then
		error(("Shader tag '%s' doesn't exist; did you add it first?"):format(tag))
	end

	---@type {[string]: string[]} # uniforms["float"] returns an array of symbols that match
	local uniforms = {}
	---@type {[string]: string[]} # varyings["float"] returns an array of symbols that match
	local varyings = {}
	---@type {[string]: string} # Map of symbol names to their unique replacement
	local swapMap = {}
	local prefix = ("%s__"):format(tag)

	---@alias Condition {condition: string, startLine: integer, endLine: integer, checkingExist: boolean}

	---@type Condition[]
	local conditionBlocks = {}

	---@type Condition[]
	local conditionStack = {}

	---@type string[] # The new contents
	local parsedLines = {}

	---@type integer[] # Sets of 2 points that mark a comment. Start is inclusive, end is exclusive.
	local commentBlocks = {}

	---@param point integer
	---@return boolean commented
	local function isCommentedRef(point)
		return isCommented(commentBlocks, point)
	end

	local lineIndex = 0
	for lineCharPosition, line in contents:gmatch("()(.-)\n") do
		---@cast lineCharPosition integer
		---@cast line string

		lineIndex = lineIndex + 1

		-- For each line...
		-- print(lineIndex, lineCharPosition, line)

		do
			-- // Look for full-line comments
			local commentStart, commentEnd = line:match("//().*()")
			if commentStart and commentBlocks[#commentBlocks] ~= math.huge then
				-- Comment exists, and there isn't a comment block
				commentBlocks[#commentBlocks+1], commentBlocks[#commentBlocks+2] =
					lineCharPosition + commentStart, lineCharPosition + commentEnd
				if line:match("^%s*//") then
					-- Entire line is commented; skip
					goto continue
				end
			end
		end

		do
			-- /* Look for comment blocks */
			local character = 1
			while character ~= #line do
				local detectingStart = commentBlocks[#commentBlocks] ~= math.huge

				if detectingStart then
					local start = line:match("/%*()", character)
					if start then
						commentBlocks[#commentBlocks+1], commentBlocks[#commentBlocks+2] =
							lineCharPosition + start, math.huge
						character = start
					else
						break
					end
				else
					local close = line:match("%*/()", character)
					if close then
						commentBlocks[#commentBlocks] = lineCharPosition + close
						character = close
					else
						break
					end
				end
			end
		end

		do
			-- Detect uniforms/varying
			-- The following examples will be correctly detected:
			-- * uniform float time, strength;
			-- * uniform vec4 offset;
			-- * varying vec3 vpos;
			for characterPosition, decType, varType, symbolNames in line:gmatch("()(%w+)%s+(%w+)%s+([_%w%s,]+);") do
				if not isCommentedRef(lineCharPosition + characterPosition) then
					-- The following matches multiple names being assigned with the same type
					for symbolName in symbolNames:gmatch("([_%w]+),?%s-") do
						-- print(("'%s'"):format(decType), ("'%s'"):format(varType), ("'%s'"):format(symbolName))

						if swapMap[symbolName] ~= nil then
							-- Duplicate declaration
							error(("Duplicate symbol '%s' declared on line %d"):format(symbolName, lineIndex))
						end

						if not reservedSymbols[symbolName] then
							-- Add the symbol and type
							if decType == "uniform" or decType == "extern" then
								local nameMap = uniforms[varType]
								if not nameMap then
									nameMap = {}
									uniforms[varType] = nameMap
								end
								nameMap[#nameMap+1] = symbolName
							elseif decType == "varying" then
								local nameMap = varyings[varType]
								if not nameMap then
									nameMap = {}
									varyings[varType] = nameMap
								end
								nameMap[#nameMap+1] = symbolName
							end
							swapMap[symbolName] = prefix..symbolName
						end
					end
				end
			end
		end

		do
			-- Detect consts
			for characterPosition, symbolName in line:gmatch("()const%s+%w+%s+([_%w]+)%s*=%s*[%w%p]+;") do
				if not isCommentedRef(lineCharPosition + characterPosition) then
					-- The following matches multiple names being assigned with the same type
					if swapMap[symbolName] ~= nil then
						-- Duplicate declaration
						error(("Duplicate symbol '%s' declared on line %d"):format(symbolName, lineIndex))
					end

					if not reservedSymbols[symbolName] then
						-- Add the symbol and type
						swapMap[symbolName] = prefix..symbolName
					end
				end
			end
		end

		do
			-- Detect functions
			-- Putting the declaration opening paranthesis on a different line will not work
			for characterPosition, returnType, funcName in line:gmatch("()(%w+)%s+([_%w]+)%s*%(") do
				if not isCommentedRef(lineCharPosition + characterPosition) then
					if swapMap[funcName] ~= nil then
						-- Duplicate declaration
						error(("Duplicate symbol '%s' declared on line %d"):format(funcName, lineIndex))
					end

					-- Add the fuction if it isn't reserved
					-- (The return type should be reserved, while the function name should not be)
					if returnType ~= "return" and reservedSymbols[returnType] and not reservedSymbols[funcName] then
						swapMap[funcName] = prefix..funcName
					end
				end
			end
		end

		do
			-- Opening #ifdef or #ifndef condition
			local conditionPosition, condType, condition = line:match("%s*()#(%w+)%s+(%w+)")
			if condition
				and
				(condType == "ifdef" or condType == "ifndef")
				and
				not isCommentedRef(lineCharPosition + conditionPosition)
			then
				-- print("==== CONDITION", condType, condition, "ON LINE", lineIndex)
				conditionStack[#conditionStack+1] = {
					condition = condition,
					startLine = lineIndex,
					endLine = 0,
					checkingExist = condType == "ifdef", -- Checking existing define
				}
				parsedLines[#parsedLines+1] = line
				goto continue
			end
		end

		do
			-- Closing #endif
			local conditionPosition = line:match("%s*()#endif")
			if conditionPosition and not isCommentedRef(lineCharPosition + conditionPosition) then
				local topmostBlock = conditionStack[#conditionStack]
				conditionStack[#conditionStack] = nil

				if not topmostBlock then
					error(("Unexpected '#endif' on line %d; missing a matching '#ifdef' or '#ifndef'"):format(lineIndex))
				end

				topmostBlock.endLine = lineIndex
				if topmostBlock.checkingExist then
					-- TODO: Add #ifndef blocks too, not just #ifdef
					conditionBlocks[#conditionBlocks+1] = topmostBlock
				end

				parsedLines[#parsedLines+1] = line
				goto continue
			end
		end

		-- Rename symbols
		local parsedLine = line:gsub("%f[_%a][_%w]+%f[%W]", swapMap)
		-- Uncomment this to see the difference between the original line and the parsed one
		-- print("---", line, "\n+++", parsedLine)
		parsedLines[#parsedLines+1] = parsedLine
		::continue::
	end

	if #conditionStack > 0 then
		-- Error if there is a missing #endif
		local block = conditionStack[1]
		error(("Missing closing '#endif' (from condition '%s' on line %d)"):format(block.condition, block.startLine))
	end

	local parsed = {
		finalSource = table.concat(parsedLines, "\n"),
		swapMap = swapMap,
		related = {},
		sentValues = {},
	}
	parsedTags[tag] = parsed
	return parsed
end

---(Creates if needed, and) returns a Shader associated with the current stack or the parameter `s`.
---Call `ShaderStack.use()` to create and set the Shader automatically.
---@param s string[]? # Stack override
---@return love.Shader shader
function ShaderStack.getShader(s)
	local stack = s or tagStack
	do
		local shader = ShaderStack.getResultShader(stack)
		if shader then return shader end
	end

	local blocks = {"#pragma language glsl3\n"}

	local vertexFuncs = {}
	local pixelFuncs = {}

	-- print("=== CREATING SHADER")
	for i = 1, #stack do
		local tag = stack[i]
		-- print(tag)

		local parsed = ShaderStack.getParsed(tag)
		if not parsed then
			error(("Shader tag '%s' doesn't exist; did you add it first?"):format(tag))
		end

		blocks[#blocks+1] = parsed.finalSource

		local vertName = parsed.swapMap.position
		if vertName then vertexFuncs[#vertexFuncs+1] = vertName end

		local pixelName = parsed.swapMap.effect
		if pixelName then pixelFuncs[#pixelFuncs+1] = pixelName end
	end

	if #vertexFuncs > 0 then
		-- Combine the vertex functions
		local vblocks = {
			"\n#ifdef VERTEX\nvec4 position(mat4 transform_projection, vec4 vertex_position) {\n\tvec4 transformed_vpos = vertex_position;",
		}

		for i = 1, #vertexFuncs do
			vblocks[#vblocks+1] = ("\ttransformed_vpos = %s(transform_projection, transformed_vpos);"):format(vertexFuncs[i])
		end

		vblocks[#vblocks+1] = "\treturn transform_projection * transformed_vpos;\n}\n#endif"
		blocks[#blocks+1] = table.concat(vblocks, "\n")
	end

	if #pixelFuncs > 0 then
		-- Combine the pixel functions
		local pblocks = {
			"\n#ifdef PIXEL\nvec4 effect(vec4 color, Image tex, vec2 texture_coords, vec2 screen_coords) {\n\tvec4 res_color = Texel(tex, texture_coords) * color;",
		}

		for i = 1, #pixelFuncs do
			pblocks[#pblocks+1] = ("\tres_color = %s(res_color, tex, texture_coords, screen_coords);"):format(pixelFuncs[i])
		end

		pblocks[#pblocks+1] = "\treturn res_color;\n}\n#endif"
		blocks[#blocks+1] = table.concat(pblocks, "\n")
	end

	local result = table.concat(blocks, "\n")
	-- Uncomment this to print the full shader's source:
	-- print(result)
	local shader = love.graphics.newShader(result)
	ShaderStack.setResultShader(tagStack, shader)

	-- Set all values in the shader
	for i = 1, #stack do
		local parsed = ShaderStack.getParsed(stack[i])
		---@cast parsed ShaderStack.Parsed
		for member, value in pairs(parsed.sentValues) do

			-- Hi! Did you get an error here related to a uniform not existing?
			-- One of your shader combinations is not reading its input; this makes the output unrelated to the previous input.
			--   * This shader is a bit later than the one the error is coming from.
			-- The shader compiler optimized out a uniform as its output was irrelevant (due to the later shader).

			-- If this behavior is intentional, you can uncomment the `if` block around `shader:send()` here \/

			-- if shader:hasUniform(parsed.swapMap[member]) then
				shader:send(parsed.swapMap[member], value)
			-- end

		end
	end

	return shader
end

---(Creates if needed, and) sets the Shader as active
---@param s string[]? # Stack override
function ShaderStack.use(s)
	love.graphics.setShader(ShaderStack.getShader(s))
end

---Resets the stack
---@param setImmediately boolean? # Should the default shader be set to active?
function ShaderStack.reset(setImmediately)
	for i = #tagStack, 1, -1 do
		tagStack[i] = nil
	end

	if (setImmediately == nil and updateOnChange) or setImmediately then
		love.graphics.setShader()
	end
end

---Sends a value to the tagged Shader's member
---@param tag string
---@param member string
---@param value any
function ShaderStack.send(tag, member, value)
	local parsed = ShaderStack.getParsed(tag)
	if not parsed then
		error(("Shader '%s' doesn't exist"):format(tag))
	end
	local related = parsed.related
	local mappedMember = parsed.swapMap[member]

	if mappedMember == nil then
		error(("Member '%s' could not be found in shader '%s'"):format(member, tag))
	end

	parsed.sentValues[member] = value
	for i = 1, #related do
		related[i]:send(mappedMember, value)
	end
end

return ShaderStack
