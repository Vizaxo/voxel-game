#version 330 core

uniform mat4 projection;

in vec4 vertexColour[];
in uint faceBitmaskB[];

layout (points) in;
//layout (triangle_strip, max_vertices = 48) out;
layout (triangle_strip, max_vertices = 48) out;

out vec4 vertColour;

const uint front  = 0x01u;
const uint back   = 0x02u;
const uint left   = 0x04u;
const uint right  = 0x08u;
const uint top    = 0x10u;
const uint bottom = 0x20u;

void main(void) {
        if ((faceBitmaskB[0] & front) != 0u) {
                // front
                gl_Position = projection * (gl_in[0].gl_Position + vec4((-0.5), (-0.5), 0.5, 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                gl_Position = projection * (gl_in[0].gl_Position + vec4(0.5, (-0.5), 0.5, 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                gl_Position = projection * (gl_in[0].gl_Position + vec4(0.5, 0.5, 0.5, 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                EndPrimitive();
                gl_Position = projection * (gl_in[0].gl_Position + vec4(0.5, 0.5, 0.5, 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                gl_Position = projection * (gl_in[0].gl_Position + vec4((-0.5), 0.5, 0.5, 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                gl_Position = projection * (gl_in[0].gl_Position + vec4((-0.5), (-0.5), 0.5, 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                EndPrimitive();
        }

        if ((faceBitmaskB[0] & back) != 0u) {
                // back
                gl_Position = projection * (gl_in[0].gl_Position + vec4((-0.5), (-0.5), (-0.5), 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                gl_Position = projection * (gl_in[0].gl_Position + vec4((-0.5), 0.5, (-0.5), 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                gl_Position = projection * (gl_in[0].gl_Position + vec4(0.5, 0.5, (-0.5), 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                EndPrimitive();
                gl_Position = projection * (gl_in[0].gl_Position + vec4(0.5, 0.5, (-0.5), 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                gl_Position = projection * (gl_in[0].gl_Position + vec4(0.5, (-0.5), (-0.5), 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                gl_Position = projection * (gl_in[0].gl_Position + vec4((-0.5), (-0.5), (-0.5), 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                EndPrimitive();
        }

        if ((faceBitmaskB[0] & left) != 0u) {
                // left
                gl_Position = projection * (gl_in[0].gl_Position + vec4((-0.5), (-0.5), (-0.5), 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                gl_Position = projection * (gl_in[0].gl_Position + vec4((-0.5), (-0.5), 0.5, 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                gl_Position = projection * (gl_in[0].gl_Position + vec4((-0.5), 0.5, 0.5, 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                EndPrimitive();
                gl_Position = projection * (gl_in[0].gl_Position + vec4((-0.5), 0.5, 0.5, 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                gl_Position = projection * (gl_in[0].gl_Position + vec4((-0.5), 0.5, (-0.5), 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                gl_Position = projection * (gl_in[0].gl_Position + vec4((-0.5), (-0.5), (-0.5), 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                EndPrimitive();
        }

        if ((faceBitmaskB[0] & right) != 0u) {
                // right
                gl_Position = projection * (gl_in[0].gl_Position + vec4(0.5, (-0.5), (-0.5), 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                gl_Position = projection * (gl_in[0].gl_Position + vec4(0.5, 0.5, (-0.5), 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                gl_Position = projection * (gl_in[0].gl_Position + vec4(0.5, 0.5, 0.5, 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                EndPrimitive();
                gl_Position = projection * (gl_in[0].gl_Position + vec4(0.5, 0.5, 0.5, 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                gl_Position = projection * (gl_in[0].gl_Position + vec4(0.5, (-0.5), 0.5, 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                gl_Position = projection * (gl_in[0].gl_Position + vec4(0.5, (-0.5), (-0.5), 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                EndPrimitive();
        }

        if ((faceBitmaskB[0] & top) != 0u) {
                // top
                gl_Position = projection * (gl_in[0].gl_Position + vec4(0.5, 0.5, 0.5, 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                gl_Position = projection * (gl_in[0].gl_Position + vec4(0.5, 0.5, (-0.5), 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                gl_Position = projection * (gl_in[0].gl_Position + vec4((-0.5), 0.5, (-0.5), 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                EndPrimitive();
                gl_Position = projection * (gl_in[0].gl_Position + vec4((-0.5), 0.5, (-0.5), 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                gl_Position = projection * (gl_in[0].gl_Position + vec4((-0.5), 0.5, 0.5, 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                gl_Position = projection * (gl_in[0].gl_Position + vec4(0.5, 0.5, 0.5, 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                EndPrimitive();
        }

        if ((faceBitmaskB[0] & bottom) != 0u) {
                // bottom
                gl_Position = projection * (gl_in[0].gl_Position + vec4(0.5, (-0.5), 0.5, 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                gl_Position = projection * (gl_in[0].gl_Position + vec4((-0.5), (-0.5), 0.5, 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                gl_Position = projection * (gl_in[0].gl_Position + vec4((-0.5), (-0.5), (-0.5), 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                EndPrimitive();
                gl_Position = projection * (gl_in[0].gl_Position + vec4((-0.5), (-0.5), (-0.5), 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                gl_Position = projection * (gl_in[0].gl_Position + vec4(0.5, (-0.5), (-0.5), 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                gl_Position = projection * (gl_in[0].gl_Position + vec4(0.5, (-0.5), 0.5, 0.0));
                vertColour = vertexColour[0];
                EmitVertex();
                EndPrimitive();
        }
}
