#version 330

out vec4 FragColor;

in vec4 vertexColour;

void main(void) {
  FragColor = vertexColour;
}
