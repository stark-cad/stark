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

// /// Type for messages sent from the context thread to the manager thread
// pub enum ContextMsg {
//     Redraw,
//     Resize(u32, u32),
//     Destroy,
// }

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
    sl_reg: usize,
    sl_tbl: usize,
    sl_env: usize,
) where
    Ij: Iterator<Item = thread::JoinHandle<()>>,
{
    let mut joins = Some(threads);

    let sl_reg = sl_reg as *mut sail::memmgt::Region;
    let sl_tbl = sl_tbl as *mut sail::SlHead;
    let sl_env = sl_env as *mut sail::SlHead;

    let prog_txt = &std::fs::read_to_string("scripts/ctxt.sl").unwrap();
    let prog_expr = sail::parser::parse(sl_reg, sl_tbl, prog_txt).unwrap();

    let mut stack = sail::eval::EvalStack::new(10000);

    stack.start_no_ret(sl_env, prog_expr);

    while !stack.is_empty() {
        stack.iter_once(sl_reg, sl_tbl);
    }

    let ctx_dst = sail::env_lookup_by_id(sl_env, sail::S_CTX_DST.0);
    let ctx_rsz = sail::env_lookup_by_id(sl_env, sail::S_CTX_RSZ.0);
    let ctx_clk = sail::env_lookup_by_id(sl_env, sail::S_CTX_CLK.0);

    let mut window_dims: [u32; 2] = [0, 0];
    let mut vk_cursor_pos: [f32; 2] = [0.0, 0.0];

    event_loop.run(move |event, _, control_flow| {
        match event {
            Event::LoopDestroyed => {
                while !stack.is_empty() {
                    stack.iter_once(sl_reg, sl_tbl);
                }

                joins.take().unwrap().for_each(|x| x.join().unwrap());
            }

            // Event::RedrawRequested(..) => {}
            Event::WindowEvent { event, .. } => match event {
                WindowEvent::CloseRequested => {
                    *control_flow = ControlFlow::Exit;

                    stack.push_frame_head(
                        stack.null_loc as *mut *mut SlHead,
                        sail::eval::Opcode::Apply,
                        sl_env,
                    );
                    stack.push(ctx_dst);
                }
                WindowEvent::Resized(dims) => {
                    *control_flow = ControlFlow::Poll;
                    window_dims = [dims.width, dims.height];

                    let w = sail::init_u32(sl_reg);
                    let h = sail::init_u32(sl_reg);
                    sail::u32_set(w, dims.width);
                    sail::u32_set(h, dims.height);

                    stack.push_frame_head(
                        stack.null_loc as *mut *mut SlHead,
                        sail::eval::Opcode::Apply,
                        sl_env,
                    );
                    stack.push(ctx_rsz);
                    stack.push(w);
                    stack.push(h);
                }
                WindowEvent::ScaleFactorChanged {
                    new_inner_size: dims,
                    ..
                } => {
                    *control_flow = ControlFlow::Poll;
                    window_dims = [dims.width, dims.height];

                    let w = sail::init_u32(sl_reg);
                    let h = sail::init_u32(sl_reg);
                    sail::u32_set(w, dims.width);
                    sail::u32_set(h, dims.width);

                    stack.push_frame_head(
                        stack.null_loc as *mut *mut SlHead,
                        sail::eval::Opcode::Apply,
                        sl_env,
                    );
                    stack.push(ctx_rsz);
                    stack.push(w);
                    stack.push(h);
                }
                WindowEvent::MouseInput { state, button, .. } => {
                    if state == event::ElementState::Pressed {
                        *control_flow = ControlFlow::Poll;

                        let x = sail::init_f32(sl_reg);
                        let y = sail::init_f32(sl_reg);
                        sail::f32_set(x, vk_cursor_pos[0]);
                        sail::f32_set(y, vk_cursor_pos[1]);

                        stack.push_frame_head(
                            stack.null_loc as *mut *mut SlHead,
                            sail::eval::Opcode::Apply,
                            sl_env,
                        );
                        stack.push(ctx_clk);
                        stack.push(x);
                        stack.push(y);
                    }
                }
                WindowEvent::CursorMoved {
                    position: dpi::PhysicalPosition { x, y },
                    ..
                } => {
                    vk_cursor_pos = [
                        (x / (window_dims[0] / 2) as f64 - 1.0) as f32,
                        (y / (window_dims[1] / 2) as f64 - 1.0) as f32,
                    ];
                }
                _ => {
                    if stack.is_empty() {
                        *control_flow = ControlFlow::Wait;
                    } else {
                        stack.iter_once(sl_reg, sl_tbl);
                    }
                }
            },

            Event::DeviceEvent { event, .. } => match event {
                DeviceEvent::Key(key) => {
                    // input_status
                    // .lock()
                    // .unwrap()
                    // .update_keys(key.scancode, key.state);
                }
                DeviceEvent::MouseWheel {
                    delta: event::MouseScrollDelta::LineDelta(xdel, ydel),
                } => {
                    // input_statu   s.lock().unwrap( ).   update_scroll(xd el, ydel);
                }
                DeviceEvent::MouseMotion {
                    delta: (xdel, ydel),
                } => {
                    // input_statu  s.lock().unwrap(  ).  update_motion(xd  el, ydel );
                }
                _ => {
                    if stack.is_empty() {
                        *control_flow = ControlFlow::Wait;
                    } else {
                        stack.iter_once(sl_reg, sl_tbl);
                    }
                }
            },

            _ => {
                if stack.is_empty() {
                    *control_flow = ControlFlow::Wait;
                } else {
                    stack.iter_once(sl_reg, sl_tbl);
                }
            }
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

// /// Window context within which STARK runs
// /// TODO: Determine whether to keep input status here
// pub struct Context {
//     pub window: Window,
//     pub event_loop: EventLoop<()>,
// }

// impl Context {
//     /// Creates a new window context with several custom parameters and an event loop
//     pub fn new(title: &str, icon_file: &str, width: u32, height: u32) -> Self {
//         let event_loop = EventLoop::new();

//         Context {
//             window: WindowBuilder::new()
//                 .with_title(title)
//                 .with_window_icon(get_icon(icon_file))
//                 .with_inner_size(dpi::Size::Physical(dpi::PhysicalSize { width, height }))
//                 .build(&event_loop)
//                 .unwrap(),
//             event_loop,
//         }
//     }

//     /// Runs the context event loop, in this case provided by `winit`
//     /// # Arguments
//     /// Takes input status to modify, a channel to send messages on,
//     /// and the handle to a manager thread to join on loop destruction
//     ///
//     /// TODO: Add handling for other kinds of window and device events
//     pub fn run(
//         self,
//         input_status: Arc<Mutex<InputStatus>>,
//         tx: mpsc::Sender<ContextMsg>,
//         manager: thread::JoinHandle<()>,
//     ) {
//         let mut manager = Some(manager);
//         self.event_loop.run(move |event, _, control_flow| {
//             *control_flow = ControlFlow::Wait;

//             match event {
//                 Event::LoopDestroyed => {
//                     tx.send(ContextMsg::Destroy).unwrap();
//                     manager.take().map(|x| x.join());
//                 }

//                 Event::RedrawRequested(..) => {
//                     tx.send(ContextMsg::Redraw).unwrap();
//                 }

//                 Event::WindowEvent { event, .. } => match event {
//                     WindowEvent::CloseRequested => {
//                         *control_flow = ControlFlow::Exit;
//                     }
//                     WindowEvent::Resized(dims) => {
//                         tx.send(ContextMsg::Resize(dims.width, dims.height))
//                             .unwrap();
//                         tx.send(ContextMsg::Redraw).unwrap();
//                     }
//                     WindowEvent::ScaleFactorChanged {
//                         new_inner_size: dims,
//                         ..
//                     } => {
//                         tx.send(ContextMsg::Resize(dims.width, dims.height))
//                             .unwrap();
//                     }
//                     WindowEvent::MouseInput { state, button, .. } => {
//                         input_status.lock().unwrap().update_mouse(button, state);
//                     }
//                     WindowEvent::CursorMoved { position, .. } => {
//                         input_status
//                             .lock()
//                             .unwrap()
//                             .update_pos(position.x as u32, position.y as u32);
//                     }
//                     _ => {}
//                 },

//                 Event::DeviceEvent { event, .. } => match event {
//                     DeviceEvent::Key(key) => {
//                         input_status
//                             .lock()
//                             .unwrap()
//                             .update_keys(key.scancode, key.state);
//                     }
//                     DeviceEvent::MouseWheel {
//                         delta: event::MouseScrollDelta::LineDelta(xdel, ydel),
//                     } => {
//                         input_status.lock().unwrap().update_scroll(xdel, ydel);
//                     }
//                     DeviceEvent::MouseMotion {
//                         delta: (xdel, ydel),
//                     } => {
//                         input_status.lock().unwrap().update_motion(xdel, ydel);
//                     }
//                     _ => {}
//                 },

//                 _ => {}
//             }
//         });
//     }
// }
