#version 430 core

layout(location = 0) in vec4 vPosition;

uniform mat4 ModelViewMatrix;
uniform mat4 ProjectionMatrix;

void
main(void)
{
  gl_Position = ProjectionMatrix * ModelViewMatrix * vPosition;
}
