#version 330 core

layout(location = 0) in vec4 vPosition;
layout(location = 1) in vec2 texture_coord;
layout(location = 2) in vec3 vOffset;
layout(location = 3) in vec3 star_colour;

uniform mat4 ModelViewMatrix;
uniform mat4 ProjectionMatrix;

out vec2 vs_tex_coord;
out vec3 vs_star_colour;

void
main(void)
{
  vec4 v = vPosition + vec4(vOffset, 0.0);
  gl_Position = ProjectionMatrix * ModelViewMatrix * v;
  vs_tex_coord = texture_coord;
  vs_star_colour = star_colour;
}
