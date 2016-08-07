#version 330 core

layout(location = 0) in vec4 vPosition;
layout(location = 1) in vec4 vColor;

uniform mat4 ModelViewMatrix;
uniform mat4 ProjectionMatrix;

out vec4 vs_color;

void
main(void)
{
  gl_Position = ProjectionMatrix * ModelViewMatrix * vPosition;
  vs_color = vColor;
}
