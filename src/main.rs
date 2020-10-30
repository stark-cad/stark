use stark;

use winit::{
    event::{DeviceEvent, Event, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
};

fn main() {
    let event_loop = EventLoop::new();

    let window = stark::create_window("STARK", "icon.png", 1280, 720, &event_loop);

    let mut inputs = stark::InputStatus::new();

    event_loop.run(move |event, _, control_flow| {
        *control_flow = ControlFlow::Wait;

        match event {
            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                window_id,
            } if window_id == window.id() => *control_flow = ControlFlow::Exit,

            Event::WindowEvent {
                event: WindowEvent::KeyboardInput { input, .. },
                window_id,
            } if window_id == window.id() => {
                // println!("Window Event: {}", input.scancode);
                // match input.virtual_keycode {
                    // Some(code) => println!("{:?}", code),
                    // None => println!("No Keycode"),
                // }
            }

            Event::DeviceEvent {
                event: DeviceEvent::Key(key),
                ..
            } => {
                inputs.update_keyboard(key.scancode, key.state);

                print!("Pressed: ");
                inputs.keys_pressed.iter().for_each(|x| print!("{} ", x));
                println!("");

                match key.virtual_keycode {
                    Some(code) => println!("{:?}", code),
                    None => println!("No Keycode"),
                }
            }

            Event::WindowEvent {
                event: WindowEvent::MouseInput { state, button, ..},
                window_id,
            } if window_id == window.id() => {
                inputs.update_mouse(button, state);
                println!("{:?} was {:?}", button, state);
            }

            Event::DeviceEvent {
                event: DeviceEvent::MouseWheel { delta },
                ..
            } => {
                println!("{:?}", delta);
            }

            Event::DeviceEvent {
                event: DeviceEvent::MouseMotion { delta: (xdel, ydel) },
                ..
            } => {
                println!("Delta x: {}; Delta y: {}", xdel, ydel);
            }

            Event::WindowEvent {
                event: WindowEvent::CursorMoved { position, .. },
                window_id,
            } if window_id == window.id() => {
                inputs.update_pos(position.x as u32, position.y as u32);
                println!("Position: x: {} y: {}", inputs.cursor_pos.x, inputs.cursor_pos.y);
            }
            _ => (),
        }
    });
}
