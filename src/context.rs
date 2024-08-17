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

// use png;
use winit::{
    dpi,
    event::{DeviceEvent, ElementState, Event, KeyEvent, RawKeyEvent, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    keyboard::{Key, KeyCode, NamedKey, PhysicalKey},
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
    let event_loop = EventLoop::new().unwrap();
    let frame = WindowBuilder::new()
        .with_title(title)
        .with_window_icon(get_icon(icon_file))
        .with_inner_size(dpi::Size::Physical(dpi::PhysicalSize { width, height }))
        .build(&event_loop)
        .unwrap();

    event_loop.set_control_flow(ControlFlow::Wait);

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

    let mut frame_dims: [u32; 2] = [0, 0];

    let mut focus = false;

    event_loop
        .run(move |event, elwt| {
            match event {
                Event::LoopExiting => {
                    joins.take().unwrap().for_each(|x| x.join().unwrap());
                }

                Event::WindowEvent { event, .. } => match event {
                    WindowEvent::RedrawRequested => {
                        let redrw = sail::sym_init(sl_reg, sail::K_CX_REDRW.0);
                        sail::queue::queue_tx(dummy_env.clone(), rndr_tx.clone(), redrw);
                    }
                    WindowEvent::CloseRequested => {
                        elwt.exit();

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
                    WindowEvent::MouseInput {
                        state, button: _, ..
                    } => {
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

                        // TODO: in context thread, maintain one copy of
                        // each keyword symbol, that gets "dispatched"
                        // every time there is a message; the queue system
                        // copies symbols into the receiving environment

                        sail::queue::queue_tx(dummy_env.clone(), rndr_tx.clone(), moved);
                    }
                    WindowEvent::KeyboardInput {
                        event:
                            KeyEvent {
                                logical_key, state, ..
                            },
                        ..
                    } if focus && state == ElementState::Pressed => match logical_key {
                        // TODO: adjust / simplify the info sent for keypresses

                        // TODO: runtime function rebinding to any key
                        // in any mode should be viable
                        Key::Character(s) => match s.bytes().nth(0).unwrap() {
                            b'u' => {
                                // move up
                                let key_u = sail::sym_init(sl_reg, sail::K_CX_KEY_U.0);
                                sail::queue::queue_tx(dummy_env.clone(), main_tx.clone(), key_u);
                            }
                            b'd' => {
                                // move down
                                let key_d = sail::sym_init(sl_reg, sail::K_CX_KEY_D.0);
                                sail::queue::queue_tx(dummy_env.clone(), main_tx.clone(), key_d);
                            }
                            b'f' => {
                                // move forward (right)
                                let key_f = sail::sym_init(sl_reg, sail::K_CX_KEY_F.0);
                                sail::queue::queue_tx(dummy_env.clone(), main_tx.clone(), key_f);
                            }
                            b'b' => {
                                // move backward (left)
                                let key_b = sail::sym_init(sl_reg, sail::K_CX_KEY_B.0);
                                sail::queue::queue_tx(dummy_env.clone(), main_tx.clone(), key_b);
                            }
                            b'l' => {
                                // make step longer
                                let key_l = sail::sym_init(sl_reg, sail::K_CX_KEY_L.0);
                                sail::queue::queue_tx(dummy_env.clone(), main_tx.clone(), key_l);
                            }
                            b's' => {
                                // make step shorter
                                let key_s = sail::sym_init(sl_reg, sail::K_CX_KEY_S.0);
                                sail::queue::queue_tx(dummy_env.clone(), main_tx.clone(), key_s);
                            }
                            b'e' => {
                                // escape line in progress
                                let key_e = sail::sym_init(sl_reg, sail::K_CX_KEY_E.0);
                                sail::queue::queue_tx(dummy_env.clone(), main_tx.clone(), key_e);
                            }
                            b'k' => {
                                // kill last line drawn
                                let key_k = sail::sym_init(sl_reg, sail::K_CX_KEY_K.0);
                                sail::queue::queue_tx(dummy_env.clone(), main_tx.clone(), key_k);
                            }
                            b'm' => {
                                // switch drawing mode
                                let key_m = sail::sym_init(sl_reg, sail::K_CX_KEY_M.0);
                                sail::queue::queue_tx(dummy_env.clone(), main_tx.clone(), key_m);
                            }
                            _ => {}
                        },
                        Key::Named(n) => match n {
                            NamedKey::Space => {
                                // enter the point
                                let recrd = sail::sym_init(sl_reg, sail::K_CX_RECRD.0);
                                sail::queue::queue_tx(dummy_env.clone(), main_tx.clone(), recrd);
                            }
                            _ => {}
                        },
                        _ => {}
                    },
                    _ => {}
                },

                // Event::DeviceEvent { event, .. } => match event {
                //     DeviceEvent::Key(RawKeyEvent {
                //         physical_key: PhysicalKey::Code(kc),
                //         state,
                //     }) => {
                //         if focus && state == ElementState::Pressed {
                //             match kc {
                //                 KeyCode::KeyU => {
                //                     // move up
                //                     let key_u = sail::sym_init(sl_reg, sail::K_CX_KEY_U.0);
                //                     sail::queue::queue_tx(
                //                         dummy_env.clone(),
                //                         main_tx.clone(),
                //                         key_u,
                //                     );
                //                 }
                //                 KeyCode::KeyD => {
                //                     // move down
                //                     let key_d = sail::sym_init(sl_reg, sail::K_CX_KEY_D.0);
                //                     sail::queue::queue_tx(
                //                         dummy_env.clone(),
                //                         main_tx.clone(),
                //                         key_d,
                //                     );
                //                 }
                //                 KeyCode::KeyF => {
                //                     // move forward (right)
                //                     let key_f = sail::sym_init(sl_reg, sail::K_CX_KEY_F.0);
                //                     sail::queue::queue_tx(
                //                         dummy_env.clone(),
                //                         main_tx.clone(),
                //                         key_f,
                //                     );
                //                 }
                //                 KeyCode::KeyB => {
                //                     // move backward (left)
                //                     let key_b = sail::sym_init(sl_reg, sail::K_CX_KEY_B.0);
                //                     sail::queue::queue_tx(
                //                         dummy_env.clone(),
                //                         main_tx.clone(),
                //                         key_b,
                //                     );
                //                 }
                //                 KeyCode::KeyL => {
                //                     // make step longer
                //                     let key_l = sail::sym_init(sl_reg, sail::K_CX_KEY_L.0);
                //                     sail::queue::queue_tx(
                //                         dummy_env.clone(),
                //                         main_tx.clone(),
                //                         key_l,
                //                     );
                //                 }
                //                 KeyCode::KeyS => {
                //                     // make step shorter
                //                     let key_s = sail::sym_init(sl_reg, sail::K_CX_KEY_S.0);
                //                     sail::queue::queue_tx(
                //                         dummy_env.clone(),
                //                         main_tx.clone(),
                //                         key_s,
                //                     );
                //                 }
                //                 KeyCode::KeyE => {
                //                     // escape line in progress
                //                     let key_e = sail::sym_init(sl_reg, sail::K_CX_KEY_E.0);
                //                     sail::queue::queue_tx(
                //                         dummy_env.clone(),
                //                         main_tx.clone(),
                //                         key_e,
                //                     );
                //                 }
                //                 KeyCode::KeyK => {
                //                     // kill last line drawn
                //                     let key_k = sail::sym_init(sl_reg, sail::K_CX_KEY_K.0);
                //                     sail::queue::queue_tx(
                //                         dummy_env.clone(),
                //                         main_tx.clone(),
                //                         key_k,
                //                     );
                //                 }
                //                 KeyCode::KeyM => {
                //                     // switch drawing mode
                //                     let key_m = sail::sym_init(sl_reg, sail::K_CX_KEY_M.0);
                //                     sail::queue::queue_tx(
                //                         dummy_env.clone(),
                //                         main_tx.clone(),
                //                         key_m,
                //                     );
                //                 }
                //                 KeyCode::Space => {
                //                     // enter the point
                //                     let recrd = sail::sym_init(sl_reg, sail::K_CX_RECRD.0);
                //                     sail::queue::queue_tx(
                //                         dummy_env.clone(),
                //                         main_tx.clone(),
                //                         recrd,
                //                     );
                //                 }
                //                 _ => {}
                //             }
                //         }
                //     }
                // DeviceEvent::MouseWheel {
                //     delta: event::MouseScrollDelta::LineDelta(xdel, ydel),
                // } => {}
                // DeviceEvent::MouseMotion {
                //     delta: (xdel, ydel),
                // } => {}
                //     _ => {}
                // },
                _ => {}
            }
        })
        .unwrap();
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
