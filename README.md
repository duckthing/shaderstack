# ShaderStack
ShaderStack allows you to combine multiple shaders into one. The high-level `push()` and `pop()` is very simple to
implement in an existing codebase, and shaders that you've combined manually can be used as an alternative as well.

## Caveats
The Love2D community generally recommends switching between canvases and applying each shader. However, this doesn't
work for vertex shaders. You may want to try the former method if you don't need vertex shaders, or if you can avoid
doing it often (as it can be expensive if done wrong).

Here are some downsides about ShaderStack's built-in parser:
* This library was created out of curiousity, and will be prone to changing
* GLSL3 only
	* It's only an implementation detail; it assumes one of the shaders require GLSL3 features
* Adding a base shader and creating variants are expensive (but should only occur once)
	* ...it's also not that great of an approach. It searches a line multiple times and performs string substitution.
* You need to write shaders in a specific way
	* The more syntax sugar and odd formatting there is, the more likely parsing will fail
	* You will need to go through your existing shaders and edit them for support
	* Ideal shaders will be described later
* You can't apply the same shader more than once
	* This will change in the future
* You can't send tuples to shaders
	* This will also change in the future
* Changes to the shader's source are not transparent
	* Complex shaders might perform badly
	* ...especially shaders with a control flow
* The results just might be ugly

You can avoid those problems by combining shaders manually and calling `setResultShader()`.

## Usage
Add `shaderstack.lua` to your project, and require it as normal. You can also clone the whole repository and require
the folder.

If you do the latter, you can run it as a Love2D project to see an example.
* Press 1-4 to push a shader to the stack
	* Hold `Left Shift` while pressing a number to pop a shader from the stack

### Lua API
All methods are documented. The following is an extremely basic example of using the library:
```lua
-- Require it
local ShaderStack = require "shaderstack"


-- Add the shader source and tag it
ShaderStack.addTag(
	"grayscale", -- The unique string associated with this shader
	love.filesystem.read("string", "grayscale.glsl") -- The source
	"gray", -- ...and an optional alias, in case the tag is difficult to type
)
-- You can add more than one alias if you'd like...
ShaderStack.addAlias("grayscale", "g") -- ...if you're really lazy
ShaderStack.addTag(...)
ShaderStack.addTag(...)


-- Push tags onto the stack
-- Shaders are applied from first to last
ShaderStack.push("grayscale") -- This adds "grayscale" to the stack
ShaderStack.push("gray") -- This adds "grayscale" too, however, the same shader can't be added twice


-- Once you're ready, call `use()` to combine and compile the shaders, and use them for your draw calls
-- Later calls of `use()` will use a previously combined shader
ShaderStack.use()
love.graphics.draw(...)


-- You can send values to uniforms with `send(tag, member, value)`
ShaderStack.send("blink", "TIME", love.timer.getTime())


-- Adding `true` as the second parameter to push/pop immediately calls `use()`
ShaderStack.push("blink", true)
love.graphics.draw(...)


---Call `reset()` at the end of love.draw
ShaderStack.reset()
```

### Shaders (and Combination Behavior)
Shaders work differently than normal. They receive values from a previous shader, modify it, and give it to the next
one. Shaders get turned into something similar to the following:

```glsl
// Assuming we have stack like this:
// {grayscale, rainbow, wobble, shift}

#ifdef PIXEL
vec4 grayscale_effect(vec4 color, ...) {
	return toGray(color);
}

vec4 rainbow_effect(vec4 color, ...) {
	return color * rainbow;
}

vec4 effect(vec4 color, ...) {
	vec4 pixel = color * Texel(...);
	pixel = grayscale_effect(pixel, ...);
	pixel = rainbow_effect(pixel, ...);
	return pixel;
}
#endif


#ifdef VERTEX
vec4 wobble_position(mat4 p, vec4 vertex_position) {
	return wobble(vertex_position);
}

vec4 shift_position(mat4 p, vec4 vertex_position) {
	return vertex_position + offset;
}

vec4 position(mat4 p, vec4 vertex_position)
{
	vec4 point = vertex_position;
	point = wobble_position(p, vertex_position);
	point = shift_position(p, vertex_position);
	return p * vertex_position;
}
#endif
```

The main body of the `effect` begins with the **provided color multiplied by the texture's pixel**. It passes it
through each of the other shaders and then returns it.

The main body of the `vertex` begins with the **untransformed vertex**. It passes it through each of the other shaders,
and then returns **the resulting vertex multiplied by the transform projection**. Do **not** apply the transform in
your vertex shaders.

> You may receive a strange error related to a uniform not existing under a certain combination. If the uniform exists
> when using the shader without others, it's due to a later shader discarding the input, and the compiler optimizing
> out the uniform.

You are required to format your shaders a certain way. It should look similar to the examples ones in `main.lua`.
* ALL vertex and fragment function bodies must be inside of their respective `#ifdef` and `#endif` blocks
```glsl
// OK
#ifdef PIXEL
vec4 effect(...)
{
	...
}
#endif

#ifdef VERTEX
vec4 position(...)
{
	...
}
#endif
```
* Function definitions should not have their opening parenthesis `(` on a different line
```glsl
// OK
vec4 effect(...)
{
	...
}

// OK
vec4 effect(
	...
) {
	...
}

// Bad!
vec4 effect
(...)
{
	...
}
```

## License
This project is licensed under the terms of the zlib license.
