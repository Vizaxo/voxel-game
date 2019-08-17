#version 330 core

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec4 colour;
layout (location = 2) in uint faceBitmaskA;

out vec4 vertexColour;
out uint faceBitmaskB;

void main(void) {
  gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);
  vertexColour = colour;
  faceBitmaskB = faceBitmaskA;
}
