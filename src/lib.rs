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

use raw_window_handle::{RawDisplayHandle, RawWindowHandle};

/// Graphical frame, provided by the desktop environment
type Frame = winit::window::Window;

pub mod context;
pub mod graphics;
pub mod sail;

use sail::SlHndl;

/// Handles for a frame (to pass to rendering system)
pub struct FrameHandles {
    pub window: RawWindowHandle,
    pub display: RawDisplayHandle,
}

unsafe impl Send for FrameHandles {}

/// Sail interpreter loop for the manager thread
pub fn manager_loop(frame: Frame, sl_reg: usize, sl_tbl: SlHndl, sl_ctr: SlHndl, sl_env: SlHndl) {
    let sl_reg = sl_reg as *mut sail::memmgt::Region;

    frame.set_cursor_icon(winit::window::CursorIcon::Crosshair);

    let frm_hdl =
        unsafe { SlHndl::from_raw_unchecked(sail::memmgt::alloc(sl_reg, 8, sail::T_FRM_HDL_ID.0)) };
    unsafe { sail::write_field_unchecked(frm_hdl.clone(), 0, (&frame as *const _) as u64) };
    sail::env_scope_ins_by_id(sl_reg, sl_env.clone(), sail::S_FRAME.0, frm_hdl);

    // println!(
    //     "frm_hdl -\ntype id: {}\nsize: {}\nhas size field: {}",
    //     sail::get_type_id(frm_hdl),
    //     sail::get_size(frm_hdl),
    //     sail::size_fld_p(frm_hdl)
    // );

    sail_fn! {
        let mngr_fns;
        _reg _tbl _env;

        "cursor-vis" 2 [frm_ptr, vis] {
            assert_eq!(frm_ptr.cfg_spec(), sail::Cfg::B8Other);
            let frame = unsafe { &*(sail::read_field_unchecked::<u64>(frm_ptr.clone(), 0) as *const Frame) };

            frame.set_cursor_visible(sail::bool_get(vis));

            frm_ptr
        }

        "cursor-pos" 5 [frm_ptr, w, h, x, y] {
            assert_eq!(frm_ptr.cfg_spec(), sail::Cfg::B8Other);
            let frame = unsafe { &*(sail::read_field_unchecked::<u64>(frm_ptr.clone(), 0) as *const Frame) };

            assert_eq!(w.core_type(), Some(sail::CoreType::U32));
            assert_eq!(h.core_type(), Some(sail::CoreType::U32));
            assert_eq!(x.core_type(), Some(sail::CoreType::F32));
            assert_eq!(y.core_type(), Some(sail::CoreType::F32));

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

            frm_ptr
        }
    }

    sail::insert_native_procs(sl_reg, sl_tbl.clone(), sl_env.clone(), mngr_fns);

    let prog_txt = &std::fs::read_to_string("scripts/main.sl").unwrap();
    let prog_expr = sail::parser::parse(sl_reg, sl_tbl.clone(), prog_txt, true).unwrap();

    let mut stack = sail::eval::EvalStack::new(10000);

    let mut _slot: usize = 0;
    let ret_addr = (&mut _slot as *mut usize) as *mut *mut sail::SlHead;

    stack.start(ret_addr, sl_env.clone(), prog_expr);

    while stack.iter_once(sl_reg, sl_tbl.clone()) {}

    let main =
        sail::env_lookup_by_id(sl_env.clone(), sail::S_MAIN.0).expect("main script not in env");

    stack.push_frame_head(ret_addr, sail::eval::Opcode::Apply, sl_env);
    stack.push(main);

    loop {
        if !stack.iter_once(sl_reg, sl_tbl.clone()) {
            println!("manager thread ended");
            break;
        }
    }
}
