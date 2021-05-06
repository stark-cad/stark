// STARK, a system for computer augmented design.
// Copyright (C) 2021 Matthew Rothlisberger

// STARK is free software: you can redistribute it and / or modify it
// under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.

// STARK is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// Affero General Public License for more details.

// You should have received a copy of the GNU Affero General Public
// License along with STARK (in the LICENSE file). If not, see
// <https://www.gnu.org/licenses/>.

// Find full copyright information in the top level COPYRIGHT file.

// <>

// src/lib.rs

// Top level lib: declares features and modules; contains the main
// Sail loop and some utility functions

// <>

#![feature(core_intrinsics)]
#![feature(const_fn_transmute)]

use raw_window_handle::{HasRawWindowHandle, RawWindowHandle};

type Frame = winit::window::Window;

pub mod context;
pub mod graphics;
pub mod sail;

use sail::SlHead;

pub struct FrameHandle(pub RawWindowHandle);

unsafe impl HasRawWindowHandle for FrameHandle {
    fn raw_window_handle(&self) -> RawWindowHandle {
        self.0
    }
}

unsafe impl Send for FrameHandle {}

pub fn manager_loop(frame: Frame, sl_reg: usize, sl_tbl: usize, sl_env: usize) {
    let (sl_tbl, sl_env) = (sl_tbl as *mut sail::SlHead, sl_env as *mut sail::SlHead);
    let sl_reg = sl_reg as *mut sail::memmgt::Region;

    frame.set_cursor_icon(winit::window::CursorIcon::Crosshair);

    let frm_obj = unsafe { sail::memmgt::alloc(sl_reg, 8, sail::Cfg::B8Other as u8) };
    unsafe { sail::write_field_unchecked(frm_obj, 0, (&frame as *const _) as u64) };
    sail::env_layer_ins_by_id(sl_reg, sl_env, sail::S_FRAME.0, frm_obj);

    sail_fn! {
        let mngr_fns;
        _reg _tbl _env;

        "cursor-vis" 2 [frm_ptr, vis] {
            assert_eq!(sail::get_cfg_spec(frm_ptr), sail::Cfg::B8Other);
            let frame = unsafe { &*(sail::read_field_unchecked::<u64>(frm_ptr, 0) as *const Frame) };

            frame.set_cursor_visible(sail::bool_get(vis));

            return sail::nil();

        }

        "cursor-pos" 5 [frm_ptr, w, h, x, y] {
            assert_eq!(sail::get_cfg_spec(frm_ptr), sail::Cfg::B8Other);
            let frame = unsafe { &*(sail::read_field_unchecked::<u64>(frm_ptr, 0) as *const Frame) };

            assert_eq!(sail::core_type(w), Some(sail::CoreType::U32));
            assert_eq!(sail::core_type(h), Some(sail::CoreType::U32));
            assert_eq!(sail::core_type(x), Some(sail::CoreType::F32));
            assert_eq!(sail::core_type(y), Some(sail::CoreType::F32));

            let (w, h, x, y) = (
                sail::u32_get(w),
                sail::u32_get(h),
                sail::f32_get(x),
                sail::f32_get(y),
            );

            frame
                .set_cursor_position(winit::dpi::Position::Physical(
                    winit::dpi::PhysicalPosition {
                        x: ((x + 1.0) * (w / 2) as f32) as i32,
                        y: ((y + 1.0) * (h / 2) as f32) as i32,
                    },
                ))
                .unwrap();

            return sail::nil();
        }
    }

    sail::insert_native_procs(sl_reg, sl_tbl, sl_env, mngr_fns);

    let prog_txt = &std::fs::read_to_string("scripts/main.sl").unwrap();
    let prog_expr = sail::parser::parse(sl_reg, sl_tbl, prog_txt).unwrap();

    let mut stack = sail::eval::EvalStack::new(10000);

    let sigil = 1 as *mut SlHead;

    let mut ret_slot = sigil;
    let ret_addr: *mut *mut SlHead = &mut ret_slot;

    stack.start(ret_addr, sl_env, prog_expr);

    while ret_slot == sigil {
        stack.iter_once(sl_reg, sl_tbl);
    }

    let main = sail::env_lookup_by_id(sl_env, sail::S_MAIN.0);

    stack.push_frame_head(ret_addr, sail::eval::Opcode::Apply, sl_env);
    stack.push(main);

    loop {
        stack.iter_once(sl_reg, sl_tbl);

        if stack.is_empty() {
            println!("manager thread broke");
            break;
        }
    }
}
