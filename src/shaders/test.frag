// shaders/test.frag
#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(location = 0)out vec4 fragment_color;

void main() {
    fragment_color = vec4(0.2, 0.1, 0.7, 1.0);
}
