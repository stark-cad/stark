// STARK, a system for computer augmented design.
// Copyright (C) 2021 Matthew Rothlisberger

// STARK is licensed under the terms of the GNU Affero General Public
// License. See the top level LICENSE file for the license text.

// Find full copyright information in the top level COPYRIGHT file.

// <>

// src/context.rs

// Functions to acquire a window from the desktop environment and get
// user input. A Sail stack is used to send input information to other
// threads.

// <>

use super::sail::{self, SlHead};

use png;
use winit::{
    dpi,
    event::{self, DeviceEvent, Event, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    window::{self, Window, WindowBuilder},
};

use std::fs::File;
use std::thread;

pub fn init_context(
    title: &str,
    icon_file: &str,
    width: u32,
    height: u32,
) -> (Window, EventLoop<()>) {
    let event_loop = EventLoop::new();
    let window = WindowBuilder::new()
        .with_title(title)
        .with_window_icon(get_icon(icon_file))
        .with_inner_size(dpi::Size::Physical(dpi::PhysicalSize { width, height }))
        .build(&event_loop)
        .unwrap();

    (window, event_loop)
}

pub fn run_loop<Ij: 'static>(
    event_loop: EventLoop<()>,
    threads: Ij,
    m_send: usize,
    r_send: usize,
    sl_reg: usize,
) where
    Ij: Iterator<Item = thread::JoinHandle<()>>,
{
    let mut joins = Some(threads);

    let stdin = thread::Builder::new()
        .name("stdin".to_string())
        .spawn(move || {
            let sl_reg = sl_reg as *mut sail::memmgt::Region;
            let main_tx = m_send as *mut sail::SlHead;

            let mut buffer = String::new();

            loop {
                buffer.clear();
                std::io::stdin().read_line(&mut buffer).unwrap();

                // println!("Got: {}", buffer);

                let strin = sail::string_init(sl_reg, &buffer);
                let shell = sail::sym_init(sl_reg, sail::K_CX_SHELL.0);

                sail::queue::queue_tx(main_tx, shell);
                sail::queue::queue_tx(main_tx, strin);
            }
        })
        .unwrap();

    let sl_reg = sl_reg as *mut sail::memmgt::Region;

    let main_tx = m_send as *mut sail::SlHead;
    let rndr_tx = r_send as *mut sail::SlHead;

    // TODO: use a small region with space for a queue sender;
    // allocate values to send in region, send them, and then
    // deallocate them to leave space for more

    // TODO: this will allow this thread and the stdin thread to be
    // much simpler

    let mut window_dims: [u32; 2] = [0, 0];
    let mut vk_cursor_pos: [f32; 2] = [0.0, 0.0];

    event_loop.run(move |event, _, control_flow| {
        *control_flow = ControlFlow::Wait;

        match event {
            Event::LoopDestroyed => {
                joins.take().unwrap().for_each(|x| x.join().unwrap());
            }

            Event::RedrawRequested(..) => {
                // println!("context redraw");

                let redrw = sail::sym_init(sl_reg, sail::K_CX_REDRW.0);

                sail::queue::queue_tx(rndr_tx, redrw);
            }

            Event::WindowEvent { event, .. } => match event {
                WindowEvent::CloseRequested => {
                    *control_flow = ControlFlow::Exit;

                    // println!("context destroy");

                    let destr = sail::sym_init(sl_reg, sail::K_CX_DESTR.0);

                    sail::queue::queue_tx(main_tx, destr);
                    sail::queue::queue_tx(rndr_tx, destr);
                }
                WindowEvent::Resized(dims) => {
                    // println!("context resize");

                    window_dims = [dims.width, dims.height];

                    let w = sail::u32_init(sl_reg, dims.width);
                    let h = sail::u32_init(sl_reg, dims.height);
                    let resiz = sail::sym_init(sl_reg, sail::K_CX_RESIZ.0);

                    sail::queue::queue_tx(rndr_tx, resiz);
                    sail::queue::queue_tx(rndr_tx, w);
                    sail::queue::queue_tx(rndr_tx, h);
                }
                WindowEvent::ScaleFactorChanged {
                    new_inner_size: dims,
                    ..
                } => {
                    // println!("context resize");

                    window_dims = [dims.width, dims.height];

                    let w = sail::u32_init(sl_reg, dims.width);
                    let h = sail::u32_init(sl_reg, dims.height);
                    let resiz = sail::sym_init(sl_reg, sail::K_CX_RESIZ.0);

                    sail::queue::queue_tx(rndr_tx, resiz);
                    sail::queue::queue_tx(rndr_tx, w);
                    sail::queue::queue_tx(rndr_tx, h);
                }
                WindowEvent::MouseInput { state, button, .. } => {
                    if state == ElementState::Pressed {
                        // println!("context click");

                        // println!("ctxt point: {:?}", vk_cursor_pos);

                        let x = sail::f32_init(sl_reg, vk_cursor_pos[0]);
                        let y = sail::f32_init(sl_reg, vk_cursor_pos[1]);
                        let click = sail::sym_init(sl_reg, sail::K_CX_CLICK.0);

                        sail::queue::queue_tx(main_tx, click);
                        sail::queue::queue_tx(main_tx, x);
                        sail::queue::queue_tx(main_tx, y);
                    }
                }
                WindowEvent::CursorMoved {
                    position: dpi::PhysicalPosition { x, y },
                    ..
                } => {
                    // TODO: add tracking line that shows next line position

                    vk_cursor_pos = [
                        (x / (window_dims[0] / 2) as f64 - 1.0) as f32,
                        (y / (window_dims[1] / 2) as f64 - 1.0) as f32,
                    ];
                }
                _ => {}
            },

            Event::DeviceEvent { event, .. } => match event {
                DeviceEvent::Key(event::KeyboardInput {
                    scancode, state, ..
                }) => {}
                DeviceEvent::MouseWheel {
                    delta: event::MouseScrollDelta::LineDelta(xdel, ydel),
                } => {}
                DeviceEvent::MouseMotion {
                    delta: (xdel, ydel),
                } => {}
                _ => {}
            },

            _ => {}
        }
    });
}

/// Retrieves an icon from a PNG file and outputs it in the format desired by Winit
fn get_icon(filename: &str) -> Option<window::Icon> {
    let decoder = png::Decoder::new(File::open(filename).unwrap());
    let (info, mut reader) = decoder.read_info().unwrap();

    let mut buf = vec![0; info.buffer_size()];

    reader.next_frame(&mut buf).unwrap();

    Some(window::Icon::from_rgba(buf, info.width, info.height).unwrap())
}
