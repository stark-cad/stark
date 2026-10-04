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

#![feature(arbitrary_self_types)]

use stark::{FrameHandles, context, graphics, manager_loop, sail};

use raw_window_handle::{HasDisplayHandle, HasWindowHandle};

use std::env;
use std::io;
use std::thread;

// TODO: have a static base Sail environment so that native functions
// may be added from anywhere in the program?

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

    sail::memmgt::init();

    // TODO: actual argument recognition
    // TODO: return exit code from main

    // cargo run file <filename> to run a Sail file
    // cargo run repl for Sail REPL
    let args: Vec<String> = env::args().collect();
    if args.len() >= 3 {
        match run_file(&args[2]) {
            Ok(out) => println!("{}", out),
            Err(err) => println!("{:?}", err),
        }
        std::process::exit(0)
    } else if args.len() >= 2 {
        repl(io::stdin())
    }

    let (frame, event_loop) = context::init_context(NAME, ICON, SIZE[0], SIZE[1]);

    let handles = FrameHandles {
        window: frame.window_handle().unwrap().as_raw(),
        display: frame.display_handle().unwrap().as_raw(),
    };

    let mut global_interact = sail::thread::Tact::create(251);
    sail::global_ctx_setup(sail::Hdl::new(&raw mut global_interact));

    // data structure that accounts for the global interaction state,
    // as well as all threads, using an array of pointers to vessels
    // NOTE: global source debug data collection switch currently here
    let mut global_weft = sail::thread::Weft::create(global_interact, true);

    let main_vsl = sail::thread::Vessel::summon(&mut global_weft, 10000, 20);
    let main_qin = main_vsl.inlet_hdl();

    let rndr_vsl = main_vsl.spawn(None, None);
    let rndr_qin = rndr_vsl.inlet_hdl();

    let base_mod = sail::env_make(main_vsl.region());
    sail::insert_native_procs(
        main_vsl.region(),
        main_vsl.context().symtab(),
        base_mod.clone(),
        sail::stdenv::ENVFNS,
    );

    main_vsl.bind_sym(sail::M_BASE.0, base_mod.clone());
    rndr_vsl.bind_sym(sail::M_BASE.0, base_mod);

    // global_weft.assign_special(main_vsl.id);
    // global_weft.assign_special(rndr_vsl.id);

    // TODO: match available parallelism or setting
    global_weft.raw.add_worker();
    global_weft.raw.add_worker();

    let rdr_tgt_obj = sail::send_hdl_init(main_vsl.region(), rndr_qin.clone());
    main_vsl.bind_sym(sail::S_RDR_TGT.0, rdr_tgt_obj);

    let rdr_id_obj = sail::i64_init(main_vsl.region(), rndr_vsl.id() as _);
    main_vsl.bind_sym(sail::S_RDR_ID.0, rdr_id_obj);

    let mgr_tgt_obj = sail::send_hdl_init(rndr_vsl.region(), main_qin.clone());
    rndr_vsl.bind_sym(sail::S_MGR_TGT.0, mgr_tgt_obj);

    let mgr_id_obj = sail::i64_init(rndr_vsl.region(), main_vsl.id() as _);
    rndr_vsl.bind_sym(sail::S_MGR_ID.0, mgr_id_obj);

    let fr_dims = sail::arrvec_init::<u32>(main_vsl.region(), sail::T_U32.0, 2, &[0, 0]);
    let cur_pos = sail::arrvec_init::<f32>(main_vsl.region(), sail::T_F32.0, 2, &[0.0, 0.0]);

    main_vsl.bind_sym(sail::S_FR_DIMS.0, fr_dims.clone());
    main_vsl.bind_sym(sail::S_CUR_POS.0, cur_pos.clone());

    // This thread renders to the graphical frame: the output interface
    let render = thread::Builder::new()
        .name("render".to_string())
        .spawn(move || graphics::render_loop(NAME, handles, rndr_vsl))
        .unwrap();

    // This thread manages the program, treating the actual main thread as a source of user input
    let manager = thread::Builder::new()
        .name("manager".to_string())
        .spawn(move || manager_loop(frame, main_vsl))
        .unwrap();

    // let (sl_main_qin, sl_rndr_qin) = (main_qin as usize, rndr_qin as usize);

    // This loop gets input from the user and detects changes to the context
    // Completely takes over the main thread; no code after this will run
    context::run_loop(
        event_loop,
        vec![manager, render].into_iter(),
        global_weft,
        ctxt_region,
        main_qin,
        rndr_qin,
        fr_dims,
        cur_pos,
    );
}

/// Accepts an input stream and runs a read - evaluate - print loop perpetually
fn repl(stream_in: std::io::Stdin) -> ! {
    let (weft, vsl) = sail::single_thread_setup();

    let region = vsl.region();

    let mut input = String::new();
    loop {
        print!(" -> ");

        input.clear();
        stream_in.read_line(&mut input).expect("stdin read failure");

        if input.is_empty() {
            drop(weft);
            std::process::exit(0)
        }

        let expr = match sail::parser::parse_text(
            region,
            weft.raw.context().symtab(),
            input.as_bytes().to_vec(),
            None,
        ) {
            Ok(out) => out,
            Err(err) => {
                println!("{:?}\n", err);
                continue;
            }
        };

        vsl.load_direct(expr);

        while vsl.advance() {}

        println!(
            "{}\n",
            sail::debug::bundle(weft.raw.context().symtab(), vsl.result().unwrap())
        );
    }
}

/// Runs a Sail file in its own context
fn run_file(filename: &str) -> Result<String, sail::debug::SlErrCode> {
    let file = match std::fs::read_to_string(filename) {
        Ok(s) => s,
        Err(_) => return Err(sail::debug::SlErrCode::FileCouldNotRead),
    };

    sail::interpret(&file, true)
}
