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

use stark::{
    context, graphics,
    sail::{self, SlHead},
};

use std::env;
use std::io;
use std::thread;

// TODO: Have a static base Sail environment so that native functions
// may be added from anywhere?

fn main() {
    const NAME: &'static str = "STARK";
    const ICON: &'static str = "icons/icon.png";
    const SIZE: [u32; 2] = [1280, 720];

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
            Err(_) => println!("Error"),
        }
        std::process::exit(0);
    } else if args.len() >= 2 {
        sail::repl(io::stdin())
    }

    let (window, event_loop) = context::init_context(NAME, ICON, SIZE[0], SIZE[1]);

    let main_region = unsafe { sail::memmgt::acquire_mem_region(1000000) };
    let rndr_region = unsafe { sail::memmgt::acquire_mem_region(1000000) };
    let ctxt_region = unsafe { sail::memmgt::acquire_mem_region(1000) };

    let (sl_tbl, main_env, rndr_env) = {
        let (tbl, m_env) = sail::prep_environment(main_region);
        sail::environment_setup(main_region, tbl, m_env);

        let r_env = sail::env_create(rndr_region, 255);

        sail::set_next_list_elt(r_env, m_env);

        (tbl, m_env, r_env)
    };

    let (mr_send, mr_recv) = sail::queue::queue_create(main_region, rndr_region);
    let (cm_send, cm_recv) = sail::queue::queue_create(ctxt_region, main_region);
    let (cr_send, cr_recv) = sail::queue::queue_create(ctxt_region, rndr_region);

    sail::env_layer_ins_by_id(main_region, main_env, sail::S_MR_SEND.0, mr_send);
    sail::env_layer_ins_by_id(main_region, main_env, sail::S_CM_RECV.0, cm_recv);

    sail::env_layer_ins_by_id(rndr_region, rndr_env, sail::S_MR_RECV.0, mr_recv);
    sail::env_layer_ins_by_id(rndr_region, rndr_env, sail::S_CR_RECV.0, cr_recv);

    let (
        sl_tbl,
        main_region,
        rndr_region,
        ctxt_region,
        main_env,
        rndr_env,
        cm_send,
        cr_send,
    ) = (
        sl_tbl as usize,
        main_region as usize,
        rndr_region as usize,
        ctxt_region as usize,
        main_env as usize,
        rndr_env as usize,
        cm_send as usize,
        cr_send as usize,
    );

    // This thread handles all rendering to the graphical frame: the output interface
    let render = thread::Builder::new()
        .name("render".to_string())
        .spawn(move || graphics::render_loop(NAME, SIZE, &window, rndr_region, sl_tbl, rndr_env))
        .unwrap();

    // This thread manages the program, treating the actual main thread as a source of user input
    let manager = thread::Builder::new()
        .name("manager".to_string())
        .spawn(move || {
            let (sl_tbl, sl_env) = (sl_tbl as *mut sail::SlHead, main_env as *mut sail::SlHead);
            let sl_reg = main_region as *mut sail::memmgt::Region;

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
        })
        .unwrap();

    // This loop gets input from the user and detects changes to the context
    // Completely takes over the main thread; no code after this will run
    context::run_loop(
        event_loop,
        vec![manager, render].into_iter(),
        cm_send,
        cr_send,
        ctxt_region,
    );
}
