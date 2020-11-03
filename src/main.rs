use stark::{
    context::{Context, ContextMsg},
    InputStatus,
};

use gfx_hal::{
    device::Device,
    window::{Extent2D, PresentationSurface, Surface},
    Instance,
};

use log::info;

use std::mem::ManuallyDrop;
use std::sync::{mpsc, Arc, Mutex};
use std::thread;

fn main() {
    const NAME: &'static str = "STARK";
    const ICON: &'static str = "icon.png";
    const SIZE: [u32; 2] = [1280, 720];

    // TODO: add useful logging throughout the program
    simple_logger::SimpleLogger::new().init().unwrap();

    let context = Context::new(NAME, ICON, SIZE[0], SIZE[1]);
    let input_status = Arc::new(Mutex::new(InputStatus::new()));
    // this channel transmits a message to the manager on event loop destruction
    let (tx, rx) = mpsc::channel::<ContextMsg>();

    let mut surface_extent = Extent2D {
        width: SIZE[0],
        height: SIZE[1],
    };
    let mut should_configure_swapchain = true;

    // currently, this channel transmits a message to the manager on event loop destruction
    // TODO: implement a ContextMsg enum or similar for messages from event loop to manager
    let (tx, rx) = mpsc::channel::<ContextMsg>();

    // This thread manages the program, treating the actual main thread as a source of user input
    let inputs = Arc::clone(&input_status);
    let manager = thread::spawn(move || loop {
        let mut curr_stat = inputs.lock().unwrap();
        // Should check_keys() and check_mouse() return iterators / iterable?
        if let Some(keys) = curr_stat.check_keys() {
            print!("Keys Down: ");
            keys.iter().for_each(|x| print!("{} ", x));
            println!("");
        }
        if let Some(buttons) = curr_stat.check_mouse() {
            print!("Mouse Down: ");
            buttons.iter().for_each(|x| print!("{:?} ", x));
            println!("");
        }
        if let Some(pos) = curr_stat.check_pos() {
            println!("Cursor Pos: x= {}, y= {}", pos.x, pos.y);
        }
        if let Some(scroll) = curr_stat.check_scroll() {
            println!("Scroll Delta: x= {}, y= {}", scroll.xdel, scroll.ydel);
        }
        if let Some(motion) = curr_stat.check_motion() {
            println!("Motion Delta: x= {}, y= {}", motion.xdel, motion.ydel);
        }

        // new main functionality goes here for now
        // render();

        match rx.try_recv() {
            Ok(ContextMsg::Destroy) => {
                info!("Manager got destruction message");
                break;
            }
            Ok(ContextMsg::Resize(width, height)) => {
                info!("Resized to {} by {}", width, height);
                surface_extent = Extent2D { width, height };
                should_configure_swapchain = true;
            }
            _ => {}
        }
    });

    // Completely takes over the main thread; no code after this will run
    context.run(input_status, tx, manager);
}


            _ => {}
        }
    });
}
