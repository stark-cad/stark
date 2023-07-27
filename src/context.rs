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

use crate::{sail, Frame};

use png;
use winit::{
    dpi,
    event::{self, DeviceEvent, ElementState, Event, VirtualKeyCode, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    window::{self, WindowBuilder},
};

use std::fs::File;
use std::thread;

/// Uses `winit` to acquire a graphical frame and create an event loop for it
pub fn init_context(
    title: &str,
    icon_file: &str,
    width: u32,
    height: u32,
) -> (Frame, EventLoop<()>) {
    let event_loop = EventLoop::new();
    let frame = WindowBuilder::new()
        .with_title(title)
        .with_window_icon(get_icon(icon_file))
        .with_inner_size(dpi::Size::Physical(dpi::PhysicalSize { width, height }))
        .build(&event_loop)
        .unwrap();

    (frame, event_loop)
}

/// Serves as the main loop for the context thread; occupies a `winit` event loop
pub fn run_loop<Ij: 'static>(
    event_loop: EventLoop<()>,
    threads: Ij,
    sl_reg: usize,
    m_send: sail::SlHndl,
    r_send: sail::SlHndl,
    fr_dims: sail::SlHndl,
    cur_pos: sail::SlHndl,
) where
    Ij: Iterator<Item = thread::JoinHandle<()>>,
{
    let mut joins = Some(threads);

    let dummy_env = sail::env_create(sl_reg as _, None);
    let dm_env_ax = dummy_env.clone();

    let main_tx = m_send.clone();

    let _stdin = thread::Builder::new()
        .name("stdin".to_string())
        .spawn(move || {
            let sl_reg = sl_reg as *mut sail::memmgt::Region;

            let mut buffer = String::new();

            loop {
                buffer.clear();
                std::io::stdin().read_line(&mut buffer).unwrap();

                let strin = sail::string_init(sl_reg, &buffer);
                let shell = sail::sym_init(sl_reg, sail::K_CX_SHELL.0);

                sail::queue::queue_tx(dm_env_ax.clone(), main_tx.clone(), shell);
                sail::queue::queue_tx(dm_env_ax.clone(), main_tx.clone(), strin);
            }
        })
        .unwrap();

    let sl_reg = sl_reg as *mut sail::memmgt::Region;

    let main_tx = m_send;
    let rndr_tx = r_send;

    // TODO: use a small region with space for a queue sender;
    // allocate objects to send in region, send them, and then
    // deallocate them to leave space for more

    // TODO: this will allow this thread and the stdin thread to be
    // much simpler

    let mut frame_dims: [u32; 2] = [0, 0];

    let mut focus = false;

    event_loop.run(move |event, _, control_flow| {
        *control_flow = ControlFlow::Wait;

        match event {
            Event::LoopDestroyed => {
                joins.take().unwrap().for_each(|x| x.join().unwrap());
            }

            Event::RedrawRequested(..) => {
                let redrw = sail::sym_init(sl_reg, sail::K_CX_REDRW.0);
                sail::queue::queue_tx(dummy_env.clone(), rndr_tx.clone(), redrw);
            }

            Event::RedrawEventsCleared => {
                // let redrw = sail::sym_init(sl_reg, sail::K_CX_REDRW.0);
                // sail::queue::queue_tx(rndr_tx.clone(), redrw);
            }

            Event::WindowEvent { event, .. } => match event {
                WindowEvent::CloseRequested => {
                    *control_flow = ControlFlow::Exit;

                    let destr = sail::sym_init(sl_reg, sail::K_CX_DESTR.0);

                    sail::queue::queue_tx(dummy_env.clone(), rndr_tx.clone(), destr.clone());
                    sail::queue::queue_tx(dummy_env.clone(), main_tx.clone(), destr);
                }
                WindowEvent::Focused(f) => {
                    focus = f;
                }
                WindowEvent::Resized(dims) => {
                    frame_dims = [dims.width, dims.height];
                    sail::arrvec_rplc(fr_dims.clone(), &[dims.width, dims.height]);

                    let resiz = sail::sym_init(sl_reg, sail::K_CX_RESIZ.0);
                    sail::queue::queue_tx(dummy_env.clone(), rndr_tx.clone(), resiz);
                }
                WindowEvent::ScaleFactorChanged {
                    new_inner_size: dims,
                    ..
                } => {
                    frame_dims = [dims.width, dims.height];
                    sail::arrvec_rplc(fr_dims.clone(), &[dims.width, dims.height]);

                    let resiz = sail::sym_init(sl_reg, sail::K_CX_RESIZ.0);
                    sail::queue::queue_tx(dummy_env.clone(), rndr_tx.clone(), resiz);
                }
                WindowEvent::MouseInput { state, button, .. } => {
                    if state == ElementState::Pressed {
                        let recrd = sail::sym_init(sl_reg, sail::K_CX_RECRD.0);
                        sail::queue::queue_tx(dummy_env.clone(), main_tx.clone(), recrd);
                    }
                }
                WindowEvent::CursorMoved {
                    position: dpi::PhysicalPosition { x, y },
                    ..
                } => {
                    sail::arrvec_rplc(
                        cur_pos.clone(),
                        &[
                            (x / (frame_dims[0] / 2) as f64 - 1.0) as f32,
                            (y / (frame_dims[1] / 2) as f64 - 1.0) as f32,
                        ],
                    );

                    let moved = sail::sym_init(sl_reg, sail::K_CX_CURMV.0);

                    // TODO: intern symbols by default, copy out into
                    // lists as necessary

                    sail::queue::queue_tx(dummy_env.clone(), rndr_tx.clone(), moved);
                }
                _ => {}
            },

            Event::DeviceEvent { event, .. } => match event {
                DeviceEvent::Key(event::KeyboardInput {
                    scancode,
                    state,
                    virtual_keycode,
                    ..
                }) => {
                    if focus && state == ElementState::Pressed {
                        match virtual_keycode {
                            Some(VirtualKeyCode::U) => {
                                // move up
                                let key_u = sail::sym_init(sl_reg, sail::K_CX_KEY_U.0);
                                sail::queue::queue_tx(dummy_env.clone(), main_tx.clone(), key_u);
                            }
                            Some(VirtualKeyCode::D) => {
                                // move down
                                let key_d = sail::sym_init(sl_reg, sail::K_CX_KEY_D.0);
                                sail::queue::queue_tx(dummy_env.clone(), main_tx.clone(), key_d);
                            }
                            Some(VirtualKeyCode::F) => {
                                // move forward (right)
                                let key_f = sail::sym_init(sl_reg, sail::K_CX_KEY_F.0);
                                sail::queue::queue_tx(dummy_env.clone(), main_tx.clone(), key_f);
                            }
                            Some(VirtualKeyCode::B) => {
                                // move backward (left)
                                let key_b = sail::sym_init(sl_reg, sail::K_CX_KEY_B.0);
                                sail::queue::queue_tx(dummy_env.clone(), main_tx.clone(), key_b);
                            }
                            Some(VirtualKeyCode::L) => {
                                // make step longer
                                let key_l = sail::sym_init(sl_reg, sail::K_CX_KEY_L.0);
                                sail::queue::queue_tx(dummy_env.clone(), main_tx.clone(), key_l);
                            }
                            Some(VirtualKeyCode::S) => {
                                // make step shorter
                                let key_s = sail::sym_init(sl_reg, sail::K_CX_KEY_S.0);
                                sail::queue::queue_tx(dummy_env.clone(), main_tx.clone(), key_s);
                            }
                            Some(VirtualKeyCode::E) => {
                                // escape line in progress
                                let key_e = sail::sym_init(sl_reg, sail::K_CX_KEY_E.0);
                                sail::queue::queue_tx(dummy_env.clone(), main_tx.clone(), key_e);
                            }
                            Some(VirtualKeyCode::K) => {
                                // kill last line drawn
                                let key_k = sail::sym_init(sl_reg, sail::K_CX_KEY_K.0);
                                sail::queue::queue_tx(dummy_env.clone(), main_tx.clone(), key_k);
                            }
                            Some(VirtualKeyCode::M) => {
                                // switch drawing mode
                                let key_m = sail::sym_init(sl_reg, sail::K_CX_KEY_M.0);
                                sail::queue::queue_tx(dummy_env.clone(), main_tx.clone(), key_m);
                            }
                            Some(VirtualKeyCode::Space) => {
                                // enter the point
                                let recrd = sail::sym_init(sl_reg, sail::K_CX_RECRD.0);
                                sail::queue::queue_tx(dummy_env.clone(), main_tx.clone(), recrd);
                            }
                            _ => {}
                        }
                    }
                }
                // DeviceEvent::MouseWheel {
                //     delta: event::MouseScrollDelta::LineDelta(xdel, ydel),
                // } => {}
                // DeviceEvent::MouseMotion {
                //     delta: (xdel, ydel),
                // } => {}
                _ => {}
            },

            _ => {}
        }
    });
}

/// Retrieves an icon from a PNG file and outputs it in the format desired by `winit`
fn get_icon(filename: &str) -> Option<window::Icon> {
    let decoder = png::Decoder::new(File::open(filename).unwrap());
    let mut reader = decoder.read_info().unwrap();
    let (w, h) = (reader.info().width, reader.info().height);
    let mut buf = vec![0; reader.output_buffer_size()];

    reader.next_frame(&mut buf).unwrap();

    Some(window::Icon::from_rgba(buf, w, h).unwrap())
}
