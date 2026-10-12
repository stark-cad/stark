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
#![feature(arbitrary_self_types)]
#![feature(core_intrinsics)]
#![feature(macro_metavar_expr)]
#![feature(ptr_mask)]
#![feature(random)]
#![feature(variant_count)]

use raw_window_handle::{RawDisplayHandle, RawWindowHandle};

/// Graphical frame, provided by the desktop environment
type Frame = winit::window::Window;

pub mod context;
pub mod graphics;
pub mod sail;

use sail::{Hdl, SlHndl, thread::Vessel};

/// Handles for a frame (to pass to rendering system)
pub struct FrameHandles {
    pub window: RawWindowHandle,
    pub display: RawDisplayHandle,
}

unsafe impl Send for FrameHandles {}

/// Sail interpreter loop for the manager vessel
pub fn manager_loop(frame: Frame, vsl_ptr: Hdl<Vessel>) {
    frame.set_cursor(winit::window::CursorIcon::Crosshair);

    crate::report_site!("manager loop frame handle alloc");
    let frm_hdl = unsafe {
        SlHndl::from_raw_unchecked(sail::memmgt::alloc(
            vsl_ptr.region(),
            8,
            sail::T_FRM_HDL_ID.0,
        ))
    };

    unsafe { sail::write_field_unchecked(frm_hdl.clone(), 0, (&frame as *const _) as u64) };

    vsl_ptr.bind_sym(sail::S_FRAME.0, frm_hdl);

    // println!(
    //     "frm_hdl -\ntype id: {}\nsize: {}\nhas size field: {}",
    //     sail::get_type_id(frm_hdl),
    //     sail::get_size(frm_hdl),
    //     sail::size_fld_p(frm_hdl)
    // );

    sail_fn! {
        let mngr_fns;
        _vsl _env;

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
        vsl_ptr.region(),
        vsl_ptr.context().symtab(),
        vsl_ptr.top_env(),
        mngr_fns,
    );

    // TODO: debug information not traced to a file here!!

    // should we have a =load_from_file= procedure, or should we have
    // a way to provide the Source of text to the parser instead of it
    // being responsible for read-in?

    vsl_ptr.load_from_file("scripts/main.sl").unwrap();

    while vsl_ptr.advance() {}

    vsl_ptr.load_proc_by_sym(sail::S_MAIN.0);

    loop {
        if !vsl_ptr.advance() {
            println!("manager thread ended");
            break;
        }
    }
}
