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

    let main_region = unsafe { sail::memmgt::acquire_mem_region(1000000) };
    let rndr_region = unsafe { sail::memmgt::acquire_mem_region(1000000) };
    let ctxt_region = unsafe { sail::memmgt::acquire_mem_region(1000) };

    let (sl_tbl, ty_ctr, main_env, rndr_env) = {
        let (tbl, ctr, m_env) = sail::prep_environment(main_region);
        sail::environment_setup(main_region, tbl.clone(), ctr.clone(), m_env.clone());

        let r_env = sail::env_create(rndr_region, Some(m_env.clone()));

        (tbl, ctr, m_env, r_env)
    };

    let (mr_send, mr_recv) = sail::queue::queue_create(main_region, rndr_region);
    let (cm_send, cm_recv) = sail::queue::queue_create(ctxt_region, main_region);
    let (cr_send, cr_recv) = sail::queue::queue_create(ctxt_region, rndr_region);

    sail::env_scope_ins_by_id(main_region, main_env.clone(), sail::S_MR_SEND.0, mr_send);
    sail::env_scope_ins_by_id(main_region, main_env.clone(), sail::S_CM_RECV.0, cm_recv);

    sail::env_scope_ins_by_id(rndr_region, rndr_env.clone(), sail::S_MR_RECV.0, mr_recv);
    sail::env_scope_ins_by_id(rndr_region, rndr_env.clone(), sail::S_CR_RECV.0, cr_recv);

    let fr_dims = sail::arrvec_init::<u32>(main_region, sail::T_U32.0, 2, &[0, 0]);
    let cur_pos = sail::arrvec_init::<f32>(main_region, sail::T_F32.0, 2, &[0.0, 0.0]);

    sail::env_scope_ins_by_id(
        main_region,
        main_env.clone(),
        sail::S_FR_DIMS.0,
        fr_dims.clone(),
    );
    sail::env_scope_ins_by_id(
        main_region,
        main_env.clone(),
        sail::S_CUR_POS.0,
        cur_pos.clone(),
    );

    let (main_region, rndr_region, ctxt_region) = (
        main_region as usize,
        rndr_region as usize,
        ctxt_region as usize,
    );

    let (sl_tb_rd, ty_ct_rd) = (sl_tbl.clone(), ty_ctr.clone());

    // This thread handles all rendering to the graphical frame: the output interface
    let render = thread::Builder::new()
        .name("render".to_string())
        .spawn(move || {
            graphics::render_loop(
                NAME,
                SIZE,
                handles,
                rndr_region,
                sl_tb_rd,
                ty_ct_rd,
                rndr_env,
            )
        })
        .unwrap();

    // This thread manages the program, treating the actual main thread as a source of user input
    let manager = thread::Builder::new()
        .name("manager".to_string())
        .spawn(move || manager_loop(frame, main_region, sl_tbl, ty_ctr, main_env))
        .unwrap();

    // This loop gets input from the user and detects changes to the context
    // Completely takes over the main thread; no code after this will run
    context::run_loop(
        event_loop,
        vec![manager, render].into_iter(),
        ctxt_region,
        cm_send,
        cr_send,
        fr_dims,
        cur_pos,
    );
}
