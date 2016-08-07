#version 330 core

in vec4 vs_color;
layout (location = 0) out vec4 color;

void
main()
{
  color = vs_color;
}
