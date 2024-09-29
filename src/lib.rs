// STARK, a system for computer augmented design.

// SPDX-FileCopyrightText: © 2020 Matthew Rothlisberger
// SPDX-License-Identifier: AGPL-3.0-only

// STARK is free software: you can redistribute it and / or modify it
// under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, version 3 of the License
// only.

// STARK is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// Affero General Public License for more details.

// You should have received a copy of the GNU Affero General Public
// License along with STARK (in the top-level LICENSES directory). If
// not, see <https://www.gnu.org/licenses/>.

// Find full copyright information in the top-level COPYRIGHT file.

// <>

// src/lib.rs

// Top level lib: declares features and modules; contains the main
// Sail loop and some utility functions

// <>

#![allow(internal_features)]
#![feature(core_intrinsics)]
#![feature(box_as_ptr)]
#![feature(const_heap)]
#![feature(macro_metavar_expr)]
#![feature(ptr_as_ref_unchecked)]

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
pub fn manager_loop(frame: Frame, sl_thr_ptr: usize) {
    let sl_thr_ptr = sl_thr_ptr as *mut sail::thread::ThreadHull;

    let thread_ref = unsafe { &mut *sl_thr_ptr };

    frame.set_cursor_icon(winit::window::CursorIcon::Crosshair);

    let frm_hdl = unsafe {
        SlHndl::from_raw_unchecked(sail::memmgt::alloc(
            thread_ref.region(),
            8,
            sail::T_FRM_HDL_ID.0,
        ))
    };

    unsafe { sail::write_field_unchecked(frm_hdl.clone(), 0, (&frame as *const _) as u64) };

    sail::env_scope_ins_by_id(
        thread_ref.region(),
        thread_ref.top_env(),
        sail::S_FRAME.0,
        frm_hdl,
    );

    // println!(
    //     "frm_hdl -\ntype id: {}\nsize: {}\nhas size field: {}",
    //     sail::get_type_id(frm_hdl),
    //     sail::get_size(frm_hdl),
    //     sail::size_fld_p(frm_hdl)
    // );

    sail_fn! {
        let mngr_fns;
        _thr _env;

        "cursor-vis" [frm_ptr, vis] {
            assert_eq!(frm_ptr.cfg_spec(), sail::Cfg::B8Other);
            let frame = unsafe { &*(sail::read_field_unchecked::<u64>(frm_ptr.clone(), 0) as *const Frame) };

            frame.set_cursor_visible(sail::bool_get(vis));

            frm_ptr
        }

        "cursor-pos" [frm_ptr, w, h, x, y] {
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

    sail::insert_native_procs(
        thread_ref.region(),
        thread_ref.context().symtab(),
        thread_ref.top_env(),
        mngr_fns,
    );

    let prog_txt = &std::fs::read_to_string("scripts/main.sl").unwrap();
    thread_ref.load_from_text(prog_txt, true).unwrap();

    while thread_ref.advance() {}

    thread_ref.load_proc_by_sym(sail::S_MAIN.0);

    loop {
        if !thread_ref.advance() {
            println!("manager thread ended");
            break;
        }
    }
}
