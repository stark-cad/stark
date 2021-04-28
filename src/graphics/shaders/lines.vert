#version 460
#extension GL_ARB_separate_shader_objects : enable


layout (location = 0) in vec2 position;

out gl_PerVertex {
    vec4 gl_Position;
};

void main() {
    gl_Position = vec4(position, 0.0, 1.0);
}
