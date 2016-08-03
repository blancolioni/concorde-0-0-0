#version 330 core

uniform sampler2D tex;

in vec2 vs_tex_coord;
in vec3 vs_star_colour;

layout (location = 0) out vec4 colour;

void
main()
{
  colour = texture(tex, vs_tex_coord);
  colour.x *= vs_star_colour.x;
  colour.y *= vs_star_colour.y;
  colour.z *= vs_star_colour.z;
}
