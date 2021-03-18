// shaders/test.vert
#version 450
#extension GL_ARB_separate_shader_objects : enable

// vec2 positions[3] = vec2[](
//                            vec2(0.0, -0.5),
//                            vec2(-0.5, 0.5),
//                            vec2(0.5, 0.5)
// );

layout (location = 0) in vec2 position;

out gl_PerVertex {
    vec4 gl_Position;
};

void main() {
    // vec2 pos = positions[gl_VertexIndex];
    gl_Position = vec4(position, 0.0, 1.0);
}
