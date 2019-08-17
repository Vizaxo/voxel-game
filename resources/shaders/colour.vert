#version 330 core

uniform mat4 projection;

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec4 colour;

out vec4 vertexColour;

void main(void) {
  gl_Position = projection * vec4(aPos.x, aPos.y, aPos.z, 1.0);
  vertexColour = colour;
}
