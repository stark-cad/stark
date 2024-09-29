// STARK, a system for computer augmented design.

// SPDX-FileCopyrightText: © 2021 Matthew Rothlisberger
// SPDX-License-Identifier: AGPL-3.0-only

// STARK is licensed under the terms of the GNU Affero General Public
// License version 3. See the top-level LICENSES directory for the
// license text.

// Find full copyright information in the top-level COPYRIGHT file.

// <>

// src/graphics/shaders/lines.frag

// Fragment shader for drawing lines in a 2D plane; colors provided
// via push constants.

// <>

#version 460
#extension GL_ARB_separate_shader_objects : enable

layout( push_constant ) uniform push_const {
    vec3 rgb_color;
} push;

layout(location = 0) out vec4 frag_color;

void main() {
    frag_color = vec4(push.rgb_color, 1.0);
}
