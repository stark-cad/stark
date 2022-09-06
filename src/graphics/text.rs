// STARK, a system for computer augmented design.
// Copyright (C) 2021 Matthew Rothlisberger

// STARK is licensed under the terms of the GNU Affero General Public
// License. See the top level LICENSE file for the license text.

// Find full copyright information in the top level COPYRIGHT file.

// <>

// src/graphics/text.rs

// To use ttf-parser, we first load a TTF / OTF font file. We provide
// a Builder function that takes point data and constructs our desired
// data structure, which will likely be an array of line endpoints.

// <>

const FONT_PATH: &str = "fonts/FreeSans.otf";

pub fn load() -> Vec<[f32; 4]> {
    let font_data = std::fs::read(FONT_PATH).unwrap();
    let face = ttf_parser::Face::from_slice(&font_data, 0).unwrap();

    let mut builder = Builder {
        acc: Vec::new(),
        last: [0.0, 0.0],
        movd: [0.0, 0.0],
    };

    let bbox = face
        .outline_glyph(face.glyph_index('A').unwrap(), &mut builder)
        .unwrap();

    let scale = (bbox.x_max - bbox.x_min) as f32;

    builder.acc.iter_mut().for_each(|l| {
        *l = [
            l[0] / scale - 0.5,
            -(l[1] / scale - 0.5),
            l[2] / scale - 0.5,
            -(l[3] / scale - 0.5),
        ]
    });

    // dbg!(bbox);
    // dbg!(builder);

    builder.acc
}

#[derive(Debug)]
struct Builder {
    acc: Vec<[f32; 4]>,
    last: [f32; 2],
    movd: [f32; 2],
}

impl ttf_parser::OutlineBuilder for Builder {
    fn move_to(&mut self, x: f32, y: f32) {
        self.last = [x, y];
        self.movd = [x, y];
    }
    fn line_to(&mut self, x: f32, y: f32) {
        self.acc.push([self.last[0], self.last[1], x, y]);
        self.last = [x, y];
    }
    fn quad_to(&mut self, _: f32, _: f32, x: f32, y: f32) {
        self.acc.push([self.last[0], self.last[1], x, y]);
        self.last = [x, y];
    }
    fn curve_to(&mut self, _: f32, _: f32, _: f32, _: f32, x: f32, y: f32) {
        self.acc.push([self.last[0], self.last[1], x, y]);
        self.last = [x, y];
    }
    fn close(&mut self) {
        self.acc
            .push([self.last[0], self.last[1], self.movd[0], self.movd[1]]);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_glyph() {
        load();
    }
}
