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

// src/main.rs

// Main function for STARK, which starts everything up. The program
// may be started with a full graphical interface, or as a Sail REPL,
// or may simply execute a Sail file.

// <>

use stark::{context, graphics, manager_loop, sail, FrameHandles};

use raw_window_handle::{HasRawDisplayHandle, HasRawWindowHandle};

use std::env;
use std::io;
use std::thread;

// TODO: Have a static base Sail environment so that native functions
// may be added from anywhere?

fn main() {
    const NAME: &'static str = "STARK";
    const ICON: &'static str = "icons/icon.png";
    const SIZE: [u32; 2] = [1280, 720];

    assert_eq!(
        sail::PTR_LEN as usize,
        std::mem::size_of::<*mut sail::SlHead>()
    );
    assert_eq!(
        std::mem::size_of::<sail::SlHndl>(),
        std::mem::size_of::<*mut sail::SlHead>()
    );

    // TODO: add useful logging throughout the program
    simple_logger::SimpleLogger::new()
        .with_level(log::LevelFilter::Debug)
        .init()
        .unwrap();

    // cargo run file <filename> to run a Sail file
    // cargo run repl for Sail REPL
    let args: Vec<String> = env::args().collect();
    if args.len() >= 3 {
        match sail::run_file(&args[2]) {
            Ok(out) => println!("{}", out),
            Err(err) => println!("{:?}", err),
        }
        std::process::exit(0);
    } else if args.len() >= 2 {
        sail::repl(io::stdin())
    }

    let (frame, event_loop) = context::init_context(NAME, ICON, SIZE[0], SIZE[1]);

    let handles = FrameHandles {
        window: frame.raw_window_handle(),
        display: frame.raw_display_handle(),
    };

    let mut global_interact = sail::thread::Tact::create(251);
    sail::global_ctx_setup(&mut global_interact);

    // data structure that accounts for the global interaction
    // state, as well as all threads, using an array of pointers
    // to pinned thread hulls
    let mut global_weft = sail::thread::Weft::create(global_interact);

    let main_thr = sail::thread::ThreadHull::summon(&global_weft, 10000, 1 << 20, None);
    sail::thread_env_setup(main_thr);
    let main_thr_ref = unsafe { &mut *main_thr };

    let main_qin = main_thr_ref.queue_inlet();

    let rndr_thr = main_thr_ref.spawn(None, None);
    let rndr_thr_ref = unsafe { &mut *rndr_thr };

    let rndr_qin = rndr_thr_ref.queue_inlet();

    let rdr_tgt_obj = sail::warp_hdl_init(main_thr_ref.region(), rndr_qin);
    sail::env_scope_ins_by_id(
        main_thr_ref.region(),
        main_thr_ref.top_env(),
        sail::S_RDR_TGT.0,
        rdr_tgt_obj,
    );

    let ctxt_region = sail::memmgt::acquire_mem_region(1000);

    let fr_dims = sail::arrvec_init::<u32>(main_thr_ref.region(), sail::T_U32.0, 2, &[0, 0]);
    let cur_pos = sail::arrvec_init::<f32>(main_thr_ref.region(), sail::T_F32.0, 2, &[0.0, 0.0]);

    sail::env_scope_ins_by_id(
        main_thr_ref.region(),
        main_thr_ref.top_env(),
        sail::S_FR_DIMS.0,
        fr_dims.clone(),
    );
    sail::env_scope_ins_by_id(
        main_thr_ref.region(),
        main_thr_ref.top_env(),
        sail::S_CUR_POS.0,
        cur_pos.clone(),
    );

    let ctxt_region = ctxt_region as usize;

    let (sl_main_thr, sl_rndr_thr) = (main_thr as usize, rndr_thr as usize);

    // This thread renders to the graphical frame: the output interface
    let render = thread::Builder::new()
        .name("render".to_string())
        .spawn(move || graphics::render_loop(NAME, SIZE, handles, sl_rndr_thr))
        .unwrap();

    // This thread manages the program, treating the actual main thread as a source of user input
    let manager = thread::Builder::new()
        .name("manager".to_string())
        .spawn(move || manager_loop(frame, sl_main_thr))
        .unwrap();

    let (sl_main_qin, sl_rndr_qin) = (main_qin as usize, rndr_qin as usize);

    // This loop gets input from the user and detects changes to the context
    // Completely takes over the main thread; no code after this will run
    context::run_loop(
        event_loop,
        vec![manager, render].into_iter(),
        global_weft,
        ctxt_region,
        sl_main_qin,
        sl_rndr_qin,
        fr_dims,
        cur_pos,
    );
}
