#version 330 core

layout(location = 0) in vec4 vPosition;
layout(location = 1) in vec3 vNormal;
layout(location = 2) in vec4 vColor;
layout(location = 3) in vec2 texture_coord;

out vec4 vs_color;
out vec2 vs_tex_coord;

uniform mat4 ModelViewMatrix;
uniform mat4 ProjectionMatrix;

void
main(void)
{
  gl_Position = ProjectionMatrix * ModelViewMatrix * vPosition;
  vs_color = vColor;
  vs_tex_coord = texture_coord;
}
