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

// TODO: Fix random crash on window resize

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
    let manager = thread::spawn(move || loop {
        let (mut x, mut y) = (0, 0);
        let (mut w, mut h) = (SIZE[0], SIZE[1]);

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
            graphics
                .draw_triangle_frame(stark::graphics::Triangle {
                    points: [
                        [-0.5, 0.5],
                        [-0.5, -0.5],
                        [
                            ((x as f32 / w as f32) * 2.0) - 1.0,
                            ((y as f32 / h as f32) * 2.0) - 1.0,
                        ],
                    ],
                })
                .unwrap();
        }
        if let Some(scroll) = curr_stat.check_scroll() {
            // println!("Scroll Delta: x= {}, y= {}", scroll.xdel, scroll.ydel);
        }
        if let Some(motion) = curr_stat.check_motion() {
            // println!("Motion Delta: x= {}, y= {}", motion.xdel, motion.ydel);
        }

        // new main functionality goes here for now

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
                graphics
                    .draw_triangle_frame(stark::graphics::Triangle {
                        points: [
                            [-0.5, 0.5],
                            [-0.5, -0.5],
                            [
                                ((x as f32 / w as f32) * 2.0) - 1.0,
                                ((y as f32 / h as f32) * 2.0) - 1.0,
                            ],
                        ],
                    })
                    .unwrap();
            }
            _ => {}
        }
    });

    // Completely takes over the main thread; no code after this will run
    context.run(input_status, tx, manager);
}
