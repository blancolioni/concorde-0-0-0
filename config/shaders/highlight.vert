#version 330 core

layout(location = 0) in vec4 vPosition;
layout(location = 1) in vec2 texture_coord;
//  layout(location = 2) in vec3 highlight_colour;

out vec2 vs_tex_coord;
out vec3 vs_highlight_colour;

uniform mat4 ModelViewMatrix;
uniform mat4 ProjectionMatrix;

void
main()
{
  gl_Position = ProjectionMatrix * ModelViewMatrix * vPosition;
  vs_tex_coord = texture_coord;
  vs_highlight_colour = vec3(0.8, 0.0, 0.8);
}
