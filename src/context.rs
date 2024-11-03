// STARK, a system for computer augmented design.

// SPDX-FileCopyrightText: © 2020 Matthew Rothlisberger
// SPDX-License-Identifier: AGPL-3.0-only

// STARK is licensed under the terms of the GNU Affero General Public
// License version 3. See the top-level LICENSES directory for the
// license text.

// Find full copyright information in the top-level COPYRIGHT file.

// <>

// src/context.rs

// Functions to acquire a window from the desktop environment and get
// user input. A Sail stack is used to send input information to other
// threads.

// <>

use crate::{sail, Frame};

// TODO: update to latest version of winit

// use png;
use winit::{
    dpi,
    event::{ElementState, Event, KeyEvent, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    keyboard::{Key, NamedKey},
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
    let frame = event_loop
        .create_window(
            Frame::default_attributes()
                .with_inner_size(dpi::Size::Physical(dpi::PhysicalSize { width, height }))
                .with_title(title)
                .with_window_icon(get_icon(icon_file)),
        )
        .unwrap();

    event_loop.set_control_flow(ControlFlow::Wait);

    (frame, event_loop)
}

/// Serves as the main loop for the context thread; occupies a `winit` event loop
pub fn run_loop<Ij: 'static>(
    event_loop: EventLoop<()>,
    threads: Ij,
    sl_weft: sail::thread::Weft,
    sl_reg: usize,
    m_send: usize,
    r_send: usize,
    fr_dims: sail::SlHndl,
    cur_pos: sail::SlHndl,
) where
    Ij: Iterator<Item = thread::JoinHandle<()>>,
{
    let mut joins = Some(threads);

    let dummy_env = sail::env_create(sl_reg as _, None);
    let dm_env_ax = dummy_env.clone();

    let _stdin = thread::Builder::new()
        .name("stdin".to_string())
        .spawn(move || {
            let sl_reg = sl_reg as *mut sail::memmgt::Region;

            let mut buffer = String::new();

            let mtx = unsafe { &mut *(m_send as *mut sail::queue::Inlet) };

            loop {
                buffer.clear();
                std::io::stdin().read_line(&mut buffer).unwrap();

                let strin = sail::string_init(sl_reg, &buffer);
                let shell = sail::sym_init(sl_reg, sail::K_CX_SHELL.0);

                sail::set_next_list_elt(dm_env_ax.clone(), shell.clone(), strin);

                mtx.transmit(1, shell);
            }
        })
        .unwrap();

    let sl_reg = sl_reg as *mut sail::memmgt::Region;

    let (main_tx, rndr_tx) = (
        m_send as *mut sail::queue::Inlet,
        r_send as *mut sail::queue::Inlet,
    );

    let (main_tx, rndr_tx) = unsafe { (&mut *main_tx, &mut *rndr_tx) };

    let mut frame_dims: [u32; 2] = [0, 0];

    let mut focus = false;

    // println!("fr_dims is at: {:x}", unsafe { fr_dims.get_raw() as usize });
    // println!("cur_pos is at: {:x}", unsafe { cur_pos.get_raw() as usize });

    event_loop
        .run(move |event, elwt| {
            match event {
                Event::LoopExiting => {
                    joins.take().unwrap().for_each(|x| x.join().unwrap());
                }

                Event::WindowEvent { event, .. } => match event {
                    WindowEvent::RedrawRequested => {
                        let redrw = sail::sym_init(sl_reg, sail::K_CX_REDRW.0);
                        rndr_tx.transmit(1, redrw);
                    }
                    WindowEvent::CloseRequested => {
                        elwt.exit();

                        let destr = sail::sym_init(sl_reg, sail::K_CX_DESTR.0);

                        rndr_tx.transmit(1, destr.clone());
                        main_tx.transmit(1, destr);
                    }
                    WindowEvent::Focused(f) => {
                        focus = f;
                    }
                    WindowEvent::Resized(dims) => {
                        frame_dims = [dims.width, dims.height];
                        sail::arrvec_rplc(fr_dims.clone(), &[dims.width, dims.height]);

                        let resiz = sail::sym_init(sl_reg, sail::K_CX_RESIZ.0);
                        rndr_tx.transmit(1, resiz);
                    }
                    WindowEvent::MouseInput {
                        state, button: _, ..
                    } => {
                        if state == ElementState::Pressed {
                            let recrd = sail::sym_init(sl_reg, sail::K_CX_RECRD.0);
                            main_tx.transmit(1, recrd);
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

                        rndr_tx.transmit(1, moved);
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
                        Key::Character(s) => {
                            let key_sym = match s.bytes().nth(0).unwrap() {
                                b'u' => {
                                    // move up
                                    Some(sail::sym_init(sl_reg, sail::K_CX_KEY_U.0))
                                }
                                b'd' => {
                                    // move down
                                    Some(sail::sym_init(sl_reg, sail::K_CX_KEY_D.0))
                                }
                                b'f' => {
                                    // move forward (right)
                                    Some(sail::sym_init(sl_reg, sail::K_CX_KEY_F.0))
                                }
                                b'b' => {
                                    // move backward (left)
                                    Some(sail::sym_init(sl_reg, sail::K_CX_KEY_B.0))
                                }
                                b'l' => {
                                    // make step longer
                                    Some(sail::sym_init(sl_reg, sail::K_CX_KEY_L.0))
                                }
                                b's' => {
                                    // make step shorter
                                    Some(sail::sym_init(sl_reg, sail::K_CX_KEY_S.0))
                                }
                                b'e' => {
                                    // escape line in progress
                                    Some(sail::sym_init(sl_reg, sail::K_CX_KEY_E.0))
                                }
                                b'k' => {
                                    // kill last line drawn
                                    Some(sail::sym_init(sl_reg, sail::K_CX_KEY_K.0))
                                }
                                b'm' => {
                                    // switch drawing mode
                                    Some(sail::sym_init(sl_reg, sail::K_CX_KEY_M.0))
                                }
                                _ => None,
                            };
                            if let Some(ks) = key_sym {
                                main_tx.transmit(1, ks)
                            }
                        }
                        Key::Named(n) => match n {
                            NamedKey::Space => {
                                // enter the point
                                let recrd = sail::sym_init(sl_reg, sail::K_CX_RECRD.0);
                                main_tx.transmit(1, recrd);
                            }
                            _ => {}
                        },
                        _ => {}
                    },
                    _ => {}
                },

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

    drop(sl_weft);
}

/// Retrieves an icon from a PNG file and outputs it in the format desired by `winit`
fn get_icon(filename: &str) -> Option<winit::window::Icon> {
    let decoder = png::Decoder::new(File::open(filename).unwrap());
    let mut reader = decoder.read_info().unwrap();
    let (w, h) = (reader.info().width, reader.info().height);
    let mut buf = vec![0; reader.output_buffer_size()];

    reader.next_frame(&mut buf).unwrap();

    Some(winit::window::Icon::from_rgba(buf, w, h).unwrap())
}
