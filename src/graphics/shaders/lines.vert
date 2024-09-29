// STARK, a system for computer augmented design.

// SPDX-FileCopyrightText: © 2021 Matthew Rothlisberger
// SPDX-License-Identifier: AGPL-3.0-only

// STARK is licensed under the terms of the GNU Affero General Public
// License version 3. See the top-level LICENSES directory for the
// license text.

// Find full copyright information in the top-level COPYRIGHT file.

// <>

// src/graphics/shaders/lines.vert

// Vertex shader for drawing simple 2D shapes; here used exclusively
// for lines.

// <>

#version 460
#extension GL_ARB_separate_shader_objects : enable

layout (location = 0) in vec2 position;

out gl_PerVertex {
    vec4 gl_Position;
};

void main() {
    gl_Position = vec4(position, 0.0, 1.0);
}
