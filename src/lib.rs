use winit::event;

use std::collections::HashSet;
use std::hash::Hash;

pub mod context;
pub mod graphics;

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
            flags: 0b00000,
            keys_pressed: HashSet::with_capacity(256),
            mouse_pressed: HashSet::with_capacity(16),
            cursor_pos: Coords { x: 0, y: 0 },
            scroll_delta: Delta {
                xdel: 0.0,
                ydel: 0.0,
            },
            motion_delta: Delta {
                xdel: 0.0,
                ydel: 0.0,
            },
        }
    }

    /// Updates the list of pressed keys with a key scancode and current state
    pub fn update_keys(&mut self, code: u32, state: event::ElementState) {
        if state == event::ElementState::Pressed {
            self.keys_pressed.insert(code);
        } else {
            self.keys_pressed.remove(&code);
        }
        self.flags |= 0b10000;
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

        self.flags |= 0b01000;
    }

    /// Updates the cursor position with a set of coordinates
    pub fn update_pos(&mut self, x: u32, y: u32) {
        self.cursor_pos = Coords { x, y };
        self.flags |= 0b00100;
    }

    pub fn update_scroll(&mut self, xdel: f32, ydel: f32) {
        self.scroll_delta = Delta { xdel, ydel };
        self.flags |= 0b00010;
    }

    pub fn update_motion(&mut self, xdel: f64, ydel: f64) {
        self.motion_delta = Delta { xdel, ydel };
        self.flags |= 0b00001;
    }

    /// Return set of pressed keys if it has changed since last check
    pub fn check_keys(&mut self) -> Option<&HashSet<u32>> {
        if self.flags & 0b10000 == 0b10000 {
            self.flags &= 0b01111;
            Some(&self.keys_pressed)
        } else {
            None
        }
    }

    /// Return set of pressed mouse buttons if it has changed since last check
    pub fn check_mouse(&mut self) -> Option<&HashSet<Mouse>> {
        if self.flags & 0b01000 == 0b01000 {
            self.flags &= 0b10111;
            Some(&self.mouse_pressed)
        } else {
            None
        }
    }

    /// Return cursor position if it has changed since last check
    pub fn check_pos(&mut self) -> Option<&Coords> {
        if self.flags & 0b00100 == 0b00100 {
            self.flags &= 0b11011;
            Some(&self.cursor_pos)
        } else {
            None
        }
    }

    pub fn check_scroll(&mut self) -> Option<&Delta<f32>> {
        if self.flags & 0b00010 == 0b00010 {
            self.flags &= 0b11101;
            Some(&self.scroll_delta)
        } else {
            None
        }
    }

    pub fn check_motion(&mut self) -> Option<&Delta<f64>> {
        if self.flags & 0b00001 == 0b00001 {
            self.flags &= 0b11110;
            Some(&self.motion_delta)
        } else {
            None
        }
    }
}
