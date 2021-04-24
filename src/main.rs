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
    const ICON: &'static str = "icon.png";
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

    // let input_status = Arc::new(Mutex::new(InputStatus::new()));
    // let inputs = Arc::clone(&input_status);

    // let (mut w, mut h) = (SIZE[0], SIZE[1]);
    // let (mut x, mut y) = (0, 0);

    let main_region = unsafe { sail::memmgt::acquire_mem_region(1000000) };
    let rndr_region = unsafe { sail::memmgt::acquire_mem_region(1000000) };
    let ctxt_region = unsafe { sail::memmgt::acquire_mem_region(1000000) };

    let (sl_tbl, main_env, rndr_env, ctxt_env) = {
        let (tbl, m_env) = sail::prep_environment(main_region);
        sail::environment_setup(main_region, tbl, m_env);

        let (r_env, c_env) = (
            sail::env_create(rndr_region, 255),
            sail::env_create(ctxt_region, 255),
        );

        sail::set_next_list_elt(r_env, m_env);
        sail::set_next_list_elt(c_env, m_env);

        (tbl, m_env, r_env, c_env)
    };

    let (mr_send, mr_recv) = sail::queue::queue_create(main_region, rndr_region);
    let (cm_send, cm_recv) = sail::queue::queue_create(ctxt_region, main_region);
    let (cr_send, cr_recv) = sail::queue::queue_create(ctxt_region, rndr_region);

    sail::env_layer_ins_by_id(main_region, main_env, sail::S_MR_SEND.0, mr_send);
    sail::env_layer_ins_by_id(main_region, main_env, sail::S_CM_RECV.0, cm_recv);

    sail::env_layer_ins_by_id(rndr_region, rndr_env, sail::S_MR_RECV.0, mr_recv);
    sail::env_layer_ins_by_id(rndr_region, rndr_env, sail::S_CR_RECV.0, cr_recv);

    sail::env_layer_ins_by_id(ctxt_region, ctxt_env, sail::S_CM_SEND.0, cm_send);
    sail::env_layer_ins_by_id(ctxt_region, ctxt_env, sail::S_CR_SEND.0, cr_send);

    let (
        sl_tbl,
        main_region,
        rndr_region,
        ctxt_region,
        main_env,
        rndr_env,
        ctxt_env,
        // mr_send,
        // mr_recv,
        // cm_send,
        // cm_recv,
        // cr_send,
        // cr_recv,
    ) = (
        sl_tbl as usize,
        main_region as usize,
        rndr_region as usize,
        ctxt_region as usize,
        main_env as usize,
        rndr_env as usize,
        ctxt_env as usize,
        // mr_send as usize,
        // mr_recv as usize,
        // cm_send as usize,
        // cm_recv as usize,
        // cr_send as usize,
        // cr_recv as usize,
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
            // let (mr_send, cm_recv) = (mr_send as *mut sail::SlHead, cm_recv as *mut sail::SlHead);

            // let prog_txt = include_str!("../scripts/main.sl");
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

            // ret_slot = sigil;

            let main = sail::env_lookup_by_id(sl_env, sail::S_MAIN.0);

            stack.push_frame_head(ret_addr, sail::eval::Opcode::Apply, sl_env);
            stack.push(main);

            loop {
                stack.iter_once(sl_reg, sl_tbl);

                if stack.is_empty() {
                    println!("manager thread broke");
                    break;
                }

                // let mut curr_stat = inputs.lock().unwrap();
                // // Should check_keys() and check_mouse() return iterators / iterable?
                // if let Some(keys) = curr_stat.check_keys() {
                //     // print!("Keys Down: ");
                //     // keys.iter().for_each(|x| print!("{} ", x));
                //     // println!("");
                // }
                // if let Some(buttons) = curr_stat.check_mouse() {
                //     // print!("Mouse Down: ");
                //     // buttons.iter().for_each(|x| print!("{:?} ", x));
                //     // println!("");
                // }
                // if let Some(pos) = curr_stat.check_pos() {
                //     // println!("Cursor Pos: x= {}, y= {}", pos.x, pos.y);
                //     x = pos.x;
                //     y = pos.y;
                // }
                // if let Some(scroll) = curr_stat.check_scroll() {
                //     // println!("Scroll Delta: x= {}, y= {}", scroll.xdel, scroll.ydel);
                // }
                // if let Some(motion) = curr_stat.check_motion() {
                //     // println!("Motion Delta: x= {}, y= {}", motion.xdel, motion.ydel);
                // }

                // new main functionality goes here for now

                // TODO: alter a rendered image as a result of running Sail
                // functions; call graphics functions from a Sail native
                // function?

                // let mut sl_input = String::new();
                // io::stdin().read_line(&mut sl_input).expect("Read Failure");

                // let sl_expr = match sail::parser::parse(sl_tbl, &sl_input) {
                //     Ok(out) => out,
                //     Err(_) => {
                //         println!("Parse Error");
                //         continue;
                //     }
                // };

                // let sl_result = match unsafe { sail::eval(sl_tbl, sl_env, sl_expr) } {
                //     Ok(out) => out,
                //     Err(_) => {
                //         println!("Evaluation Error");
                //         continue;
                //     }
                // };

                // println!("{}", sail::context(sl_tbl, sl_result).to_string());

                // match rx.try_recv() {
                //     // Last block to run in the manager thread
                //     Ok(ContextMsg::Destroy) => {
                //         info!("Manager got destruction message");
                //         break;
                //     }
                //     Ok(ContextMsg::Resize(width, height)) => {
                //         info!("Resized to {} by {}", width, height);
                //         // graphics.set_extent(width, height);
                //         // should_configure_swapchain = true;

                //         w = width;
                //         h = height;
                //     }
                //     Ok(ContextMsg::Redraw) => {
                //         info!("Received redraw message");
                //     }
                //     _ => {}
                // }
            }
        })
        .unwrap();

    // This loop gets input from the user and detects changes to the context
    // Completely takes over the main thread; no code after this will run
    context::run_loop(
        event_loop,
        vec![manager, render].into_iter(),
        ctxt_region,
        sl_tbl,
        ctxt_env,
    );
}
