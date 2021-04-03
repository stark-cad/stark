use stark::{context, graphics, sail, InputStatus};

use std::env;
use std::io;
use std::sync::{Arc, Mutex};
use std::thread;

// TODO: Have a static base Sail environment so that native functions
// may be added from anywhere?

fn main() {
    const NAME: &'static str = "STARK";
    const ICON: &'static str = "icon.png";
    const SIZE: [u32; 2] = [1280, 720];

    // TODO: add useful logging throughout the program
    simple_logger::SimpleLogger::new().init().unwrap();

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

    // let context = Context::new(NAME, ICON, SIZE[0], SIZE[1]);
    let input_status = Arc::new(Mutex::new(InputStatus::new()));
    // this channel transmits a message to the manager on event loop destruction
    // let (tx, rx) = mpsc::channel::<ContextMsg>();

    let inputs = Arc::clone(&input_status);

    let (mut w, mut h) = (SIZE[0], SIZE[1]);
    let (mut x, mut y) = (0, 0);

    // let main_sector = unsafe { sail::memmgt::acquire_mem_sector(1000000) };
    // let render_sector = unsafe { sail::memmgt::acquire_mem_sector(1000000) };
    // let context_sector = unsafe { sail::memmgt::acquire_mem_sector(100000) };

    // let (sl_tbl, sl_env) = unsafe {
    //     let tbl = sail::sym_tab_create(main_sector);
    //     let env = sail::env_create(main_sector);

    //     sail::environment_setup(tbl, env);

    //     (tbl as usize, env as usize)
    // };

    // let (mr_send, mr_recv) = unsafe { sail::queue::queue_create(main_sector, render_sector) };
    // let (cm_send, cm_recv) = unsafe { sail::queue::queue_create(context_sector, main_sector) };
    // let (cr_send, cr_recv) = unsafe { sail::queue::queue_create(context_sector, render_sector) };

    // let (
    //     main_sector,
    //     render_sector,
    //     context_sector,
    //     mr_send,
    //     mr_recv,
    //     cm_send,
    //     cm_recv,
    //     cr_send,
    //     cr_recv,
    // ) = (
    //     main_sector as usize,
    //     render_sector as usize,
    //     context_sector as usize,
    //     mr_send as usize,
    //     mr_recv as usize,
    //     cm_send as usize,
    //     cm_recv as usize,
    //     cr_send as usize,
    //     cr_recv as usize,
    // );

    // This thread handles all rendering to the graphical frame: the output interface
    let render = thread::spawn(move || {
        graphics::render_loop(NAME, SIZE, &window, 0, /*mr_recv*/ 0 /*cr_recv*/)
    });

    // This thread manages the program, treating the actual main thread as a source of user input
    let manager = thread::spawn(move || {
        // let (sl_tbl, sl_env) = (sl_tbl as *mut sail::SlHead, sl_env as *mut sail::SlHead);
        // let main_sector = main_sector as *mut sail::memmgt::MemSector;
        // let (mr_send, cm_recv) = (mr_send as *mut sail::SlHead, cm_recv as *mut sail::SlHead);

        // unsafe {
        //     let send_id = sail::init_symbol(main_sector, false, sail::SlSymbolMode::ById, 0);
        //     sail::sym_set_id(send_id, sail::sym_tab_get_id(sl_tbl, "g_queue"));
        //     sail::env_layer_ins_entry(sail::car(sl_env), send_id, mr_send);
        // }

        loop {
            let mut curr_stat = inputs.lock().unwrap();
            // Should check_keys() and check_mouse() return iterators / iterable?
            if let Some(keys) = curr_stat.check_keys() {
                // print!("Keys Down: ");
                // keys.iter().for_each(|x| print!("{} ", x));
                // println!("");
            }
            if let Some(buttons) = curr_stat.check_mouse() {
                // print!("Mouse Down: ");
                // buttons.iter().for_each(|x| print!("{:?} ", x));
                // println!("");
            }
            if let Some(pos) = curr_stat.check_pos() {
                // println!("Cursor Pos: x= {}, y= {}", pos.x, pos.y);
                x = pos.x;
                y = pos.y;
                // graphics
                //     .draw_triangle_frame(stark::graphics::Triangle {
                //         points: [
                //             [-0.5, 0.5],
                //             [-0.5, -0.5],
                //             [
                //                 ((x as f32 / w as f32) * 2.0) - 1.0,
                //                 ((y as f32 / h as f32) * 2.0) - 1.0,
                //             ],
                //         ],
                //     })
                //     .unwrap();

                // graphics
                //     .draw_clear_frame([0.0, x as f32 / w as f32, y as f32 / h as f32, 0.0])
                //     .unwrap();
            }
            if let Some(scroll) = curr_stat.check_scroll() {
                // println!("Scroll Delta: x= {}, y= {}", scroll.xdel, scroll.ydel);
            }
            if let Some(motion) = curr_stat.check_motion() {
                // println!("Motion Delta: x= {}, y= {}", motion.xdel, motion.ydel);
            }

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
            //         // graphics.draw_clear_frame([0.2, 0.1, 0.7, 1.0]).unwrap();

            //         // graphics
            //         //     .draw_triangle_frame(stark::graphics::Triangle {
            //         //         points: [
            //         //             [-0.5, 0.5],
            //         //             [-0.5, -0.5],
            //         //             [
            //         //                 ((x as f32 / w as f32) * 2.0) - 1.0,
            //         //                 ((y as f32 / h as f32) * 2.0) - 1.0,
            //         //             ],
            //         //         ],
            //         //     })
            //         //     .unwrap();
            //     }
            //     _ => {}
            // }
        }
    });

    // This loop gets input from the user and detects changes to the context
    // Completely takes over the main thread; no code after this will run
    context::run_loop(
        event_loop,
        vec![manager, render].into_iter(),
        0, //context_sector,
        0, //cm_send,
        0, //cr_send,
    );
}
