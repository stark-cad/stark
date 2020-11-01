use png;
use winit::{
    dpi, event,
    event_loop::EventLoop,
    window::{self, Window, WindowBuilder},
};

use std::collections::HashSet;
use std::fs::File;
use std::hash::Hash;

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

    /// Retrieves an icon from a PNG file and outputs it in the format desired by Winit
    fn get_icon(filename: &str) -> Option<window::Icon> {
        let decoder = png::Decoder::new(File::open(filename).unwrap());
        let (info, mut reader) = decoder.read_info().unwrap();

        let mut buf = vec![0; info.buffer_size()];

        reader.next_frame(&mut buf).unwrap();

        Some(window::Icon::from_rgba(buf, info.width, info.height).unwrap())
    }
}

/// Type for messages sent from the context thread to the manager thread
pub enum ContextMsg {
    Resize(u32, u32),
    Destroy,
}

/// Tracks the state of all relevant inputs (keys, mouse buttons, cursor position, motion)
/// TODO: Handle other inputs, such as arbitrary buttons and analog axes
pub struct InputStatus {
    /// Bit flags representing whether values have been updated since last check
    /// Five bits, one for each input type stored by this struct
    flags: u32,

    /// List of all keys currently pressed; may be limited by hardware
    keys_pressed: HashSet<u32>,

    /// List of all mouse buttons currently pressed
    mouse_pressed: HashSet<Mouse>,

    /// Current position of the cursor on the window
    cursor_pos: Coords,

    // These are sort of movement vectors input by the user
    // TODO: Maybe move things like this to a queue, accumulator, or something
    // TODO: Actually, maybe queueing of inputs should be handled on a higher level
    scroll_delta: Delta<f32>,
    motion_delta: Delta<f64>,
}

/// Coordinates on the window plane, for interacting with the UI
pub struct Coords {
    pub x: u32,
    pub y: u32,
}

/// Motion of some input device, like mouse or scroll wheel
pub struct Delta<T> {
    pub xdel: T,
    pub ydel: T,
}

/// Local enum for keeping track of mouse button presses
#[derive(Debug, Hash, PartialEq, Eq)]
pub enum Mouse {
    Left,
    Right,
    Middle,
    Other(u8),
}

impl InputStatus {
    /// Creates a new default InputStatus to hold onto
    pub fn new() -> Self {
        InputStatus {
            keys_pressed: HashSet::with_capacity(128),
            mouse_pressed: HashSet::with_capacity(16),
            cursor_pos: Coords { x: 0, y: 0 },
        }
    }

    /// Updates the list of pressed keys with a key scancode and current state
    pub fn update_keyboard(&mut self, code: u32, state: event::ElementState) {
        if state == event::ElementState::Pressed {
            self.keys_pressed.insert(code);
        } else {
            self.keys_pressed.remove(&code);
        }
    }

    /// Updates the list of pressed mouse buttons with a button value and state
    pub fn update_mouse(&mut self, button: event::MouseButton, state: event::ElementState) {
        let value: Mouse = match button {
            event::MouseButton::Left => Mouse::Left,
            event::MouseButton::Right => Mouse::Right,
            event::MouseButton::Middle => Mouse::Middle,
            event::MouseButton::Other(val) => Mouse::Other(val),
        };

        if state == event::ElementState::Pressed {
            self.mouse_pressed.insert(value);
        } else {
            self.mouse_pressed.remove(&value);
        }
    }

    /// Updates the cursor position with a set of coordinates
    pub fn update_pos(&mut self, x: u32, y: u32) {
        self.cursor_pos.x = x;
        self.cursor_pos.y = y;
    }
}

/// Creates a new window with several custom parameters and an event loop
pub fn create_window(
    title: &str,
    icon_file: &str,
    width: u32,
    height: u32,
    event_loop: &EventLoop<()>,
) -> Window {
    WindowBuilder::new()
        .with_title(title)
        .with_window_icon(get_icon(icon_file))
        .with_inner_size(dpi::Size::Physical(dpi::PhysicalSize { width, height }))
        .build(event_loop)
        .unwrap()
}

/// Retrieves an icon from a PNG file and outputs it in the format desired by Winit
fn get_icon(filename: &str) -> Option<window::Icon> {
    let decoder = png::Decoder::new(File::open(filename).unwrap());
    let (info, mut reader) = decoder.read_info().unwrap();

    let mut buf = vec![0; info.buffer_size()];

    reader.next_frame(&mut buf).unwrap();

    Some(window::Icon::from_rgba(buf, info.width, info.height).unwrap())
}
