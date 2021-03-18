use stark::{
    context::{Context, ContextMsg},
    graphics::GraphicsState,
    sail, InputStatus,
};

use log::info;

use std::env;
use std::io;
use std::sync::{mpsc, Arc, Mutex};
use std::thread;

// TODO: Have a static base Sail environment so that native functions
// may be added from anywhere?

fn main() {
    const NAME: &'static str = "STARK";
    const ICON: &'static str = "icon.png";
    const SIZE: [u32; 2] = [1280, 720];

    // TODO: add useful logging throughout the program
    simple_logger::SimpleLogger::new().init().unwrap();

    let args: Vec<String> = env::args().collect();
    if args.len() >= 2 {
        sail::repl(io::stdin())
    }

    let context = Context::new(NAME, ICON, SIZE[0], SIZE[1]);
    let input_status = Arc::new(Mutex::new(InputStatus::new()));
    // this channel transmits a message to the manager on event loop destruction
    let (tx, rx) = mpsc::channel::<ContextMsg>();

    let mut graphics: GraphicsState<backend::Backend> =
        GraphicsState::new(&context, NAME, SIZE[0], SIZE[1]);
    graphics.setup();

    let mut should_configure_swapchain = true;

    // This thread manages the program, treating the actual main thread as a source of user input
    let inputs = Arc::clone(&input_status);

    let (mut w, mut h) = (SIZE[0], SIZE[1]);
    let (mut x, mut y) = (0, 0);

    let manager = thread::spawn(move || {
        let (sl_tbl, sl_env) = unsafe {
            let tbl = sail::sym_tab_create();
            let env = sail::env_create();

            sail::environment_setup(tbl, env);

            (tbl, env)
        };

        let (g_send, g_recv) = unsafe { sail::queue::queue_create() };

        unsafe {
            let send_str = sail::init_symbol(false, sail::SlSymbolMode::ByStr, 7);
            sail::sym_set_str(send_str, b"g_queue");
            let send_id = sail::init_symbol(false, sail::SlSymbolMode::ById, 0);
            sail::sym_set_id(send_id, sail::sym_tab_insert(sl_tbl, send_str));

            sail::env_layer_ins_entry(sail::car(sl_env), send_id, g_send);
        }

        loop {
            if should_configure_swapchain {
                graphics.config_swapchain();
                should_configure_swapchain = false;
            }

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

            let mut sl_input = String::new();
            io::stdin().read_line(&mut sl_input).expect("Read Failure");

            let sl_expr = match sail::parser::parse(sl_tbl, &sl_input) {
                Ok(out) => out,
                Err(_) => {
                    println!("Parse Error");
                    continue;
                }
            };

            let sl_result = match unsafe { sail::eval(sl_tbl, sl_env, sl_expr) } {
                Ok(out) => out,
                Err(_) => {
                    println!("Evaluation Error");
                    continue;
                }
            };

            println!("{}", sail::context(sl_tbl, sl_result).to_string());

            unsafe {
                let next = sail::queue::queue_rx(g_recv);

                if sail::get_type(next) != sail::SlType::Vec
                    || sail::vec_mode(next) != sail::SlVecMode::FlatF32
                {
                    println!("garbage from queue");
                } else {
                    graphics
                        .draw_clear_frame([
                            sail::vec_idx_f32(next, 0),
                            sail::vec_idx_f32(next, 1),
                            sail::vec_idx_f32(next, 2),
                            sail::vec_idx_f32(next, 3),
                        ])
                        .unwrap();
                }
            }

            match rx.try_recv() {
                // Last block to run in the manager thread
                Ok(ContextMsg::Destroy) => {
                    info!("Manager got destruction message");
                    break;
                }
                Ok(ContextMsg::Resize(width, height)) => {
                    info!("Resized to {} by {}", width, height);
                    graphics.set_extent(width, height);
                    should_configure_swapchain = true;

                    w = width;
                    h = height;
                }
                Ok(ContextMsg::Redraw) => {
                    info!("Received redraw message");
                    // graphics.draw_clear_frame([0.2, 0.1, 0.7, 1.0]).unwrap();

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
                }
                _ => {}
            }
        }
    });

    // Completely takes over the main thread; no code after this will run
    context.run(input_status, tx, manager);
}
