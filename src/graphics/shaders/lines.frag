#version 460
#extension GL_ARB_separate_shader_objects : enable

layout( push_constant ) uniform push_const {
    vec3 rgb_color;
} push;

layout(location = 0) out vec4 frag_color;

void main() {
    frag_color = vec4(push.rgb_color, 1.0);
}
