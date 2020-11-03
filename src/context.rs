use crate::InputStatus;

use png;
use winit::{
    dpi,
    event::{self, DeviceEvent, Event, WindowEvent},
    event_loop::ControlFlow,
    event_loop::EventLoop,
    window::{self, Window, WindowBuilder},
};

use std::fs::File;
use std::sync::{mpsc, Arc, Mutex};
use std::thread;

/// Type for messages sent from the context thread to the manager thread
pub enum ContextMsg {
    Redraw,
    Resize(u32, u32),
    Destroy,
}

/// Window context within which STARK runs
/// TODO: Determine whether to keep input status here
pub struct Context {
    pub window: Window,
    pub event_loop: EventLoop<()>,
}

impl Context {
    /// Creates a new window context with several custom parameters and an event loop
    pub fn new(title: &str, icon_file: &str, width: u32, height: u32) -> Self {
        let event_loop = EventLoop::new();

        Context {
            window: WindowBuilder::new()
                .with_title(title)
                .with_window_icon(Context::get_icon(icon_file))
                .with_inner_size(dpi::Size::Physical(dpi::PhysicalSize { width, height }))
                .build(&event_loop)
                .unwrap(),
            event_loop,
        }
    }

    /// Runs the context event loop, in this case provided by `winit`
    /// # Arguments
    /// Takes input status to modify, a channel to send messages on,
    /// and the handle to a manager thread to join on loop destruction
    ///
    /// TODO: Add handling for other kinds of window and device events
    pub fn run(
        self,
        input_status: Arc<Mutex<InputStatus>>,
        tx: mpsc::Sender<ContextMsg>,
        manager: thread::JoinHandle<()>,
    ) {
        let mut manager = Some(manager);
        self.event_loop.run(move |event, _, control_flow| {
            *control_flow = ControlFlow::Wait;

            match event {
                Event::LoopDestroyed => {
                    tx.send(ContextMsg::Destroy).unwrap();
                    manager.take().map(|x| x.join());
                }

                Event::RedrawRequested(..) => {
                    tx.send(ContextMsg::Redraw).unwrap();
                }

                Event::WindowEvent { event, .. } => match event {
                    WindowEvent::CloseRequested => {
                        *control_flow = ControlFlow::Exit;
                    }
                    WindowEvent::Resized(dims) => {
                        tx.send(ContextMsg::Resize(dims.width, dims.height))
                            .unwrap();
                        tx.send(ContextMsg::Redraw).unwrap();
                    }
                    WindowEvent::ScaleFactorChanged {
                        new_inner_size: dims,
                        ..
                    } => {
                        tx.send(ContextMsg::Resize(dims.width, dims.height))
                            .unwrap();
                    }
                    WindowEvent::MouseInput { state, button, .. } => {
                        input_status.lock().unwrap().update_mouse(button, state);
                    }
                    WindowEvent::CursorMoved { position, .. } => {
                        input_status
                            .lock()
                            .unwrap()
                            .update_pos(position.x as u32, position.y as u32);
                    }
                    _ => {}
                },

                Event::DeviceEvent { event, .. } => match event {
                    DeviceEvent::Key(key) => {
                        input_status
                            .lock()
                            .unwrap()
                            .update_keys(key.scancode, key.state);
                    }
                    DeviceEvent::MouseWheel {
                        delta: event::MouseScrollDelta::LineDelta(xdel, ydel),
                    } => {
                        input_status.lock().unwrap().update_scroll(xdel, ydel);
                    }
                    DeviceEvent::MouseMotion {
                        delta: (xdel, ydel),
                    } => {
                        input_status.lock().unwrap().update_motion(xdel, ydel);
                    }
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
}
