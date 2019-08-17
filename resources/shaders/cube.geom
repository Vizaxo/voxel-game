#version 330 core

uniform mat4 projection;

in vec4 vertexColour[];

layout (points) in;
//layout (triangle_strip, max_vertices = 48) out;
layout (triangle_strip, max_vertices = 48) out;

out vec4 vertColour;

void main(void) {
        /*
        gl_Position = projection * (gl_in[0].gl_Position);
        vertColour = vertexColour[0];
        EmitVertex();
        EndPrimitive();
}
*/
        // top
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
