![img](icons/logo.png)


<a id="org64ef47f"></a>

# STARK

STARK is a prototype system for computer augmented design. It is
intended to offer designers powerful capabilities that are not
currently available, and it will serve as a malleable substrate for
work in many domains. The first target use cases are mechanical and
electrical engineering.

Much of STARK's behavior is defined in a custom scripting language
called Sail, which is a purpose-built Lisp dialect. In its current
state, STARK consists of a Sail interpreter coupled with a rendering
engine built directly out of Vulkan primitives using `gfx-hal`. At
runtime, multiple interpreter stacks run in parallel to execute the
main scripts, get user input, and send draw calls to be rendered.

At its first public release, STARK can be used as a Sail REPL and / or
a 2D line drawing and diagramming application. This is only the
beginning of the graphical capabilities STARK will offer, but it
demonstrates the feasibility and interoperability of its core systems
(context, interpreter, and renderer). Diagrams may be drawn using the
mouse or keyboard alone, both in combination, or with Sail code.


<a id="org440e468"></a>

## Caution

**STARK is still an early prototype.** It is in active development and
is not yet useful for most design tasks. Please be aware that the
application is not ready for broad usage, has numerous bugs, and is
subject to great ongoing change. That said, testing and feedback is
much appreciated. If you do choose to install STARK and try it out,
thank you!


<a id="orgbf7e4d9"></a>

## Installation

STARK is known to run on Arch Linux and Windows 10. It will presumably
run on almost all Linux distributions, and should run on macOS. It may
not work properly on processor architectures besides x86-64. Binaries
will be distributed in the future, but for now you must compile the
program yourself.


<a id="org33b9325"></a>

### Linux

Several packages need to be installed on your system in order to build
and run STARK.

1.  Required Packages

    -   `git`
    -   `rustup`
    -   `shaderc`
    -   `vulkan-tools`
    
    Also necessary is a Vulkan driver package; this may be `vulkan-intel`,
    `vulkan-radeon`, `amdvlk`, an appropriate Nvidia driver, or similar.
    
    If you cannot install the `shaderc` package, you will need `cmake` and
    `python` so the library can be built from source.

2.  Building

    At the command line, navigate to an appropriate directory and run `git
    clone https://github.com/stark-cad/stark.git`. Alternatively, you can
    clone via SSH.
    
    Navigate to the repository's base directory and open
    `Cargo.toml`. Check that "vulkan" appears twice near the end of the
    file. If not, run `/tools/set-backend.sh vulkan`.
    
    Run `vulkaninfo | grep 'Version:'`. If the version shown is below 1.2,
    you need an updated graphics driver to continue.
    
    Run `rustup update` to ensure that an up-to-date Rust toolchain is
    present. Compile STARK by running `cargo build`.
    
    When you run `cargo run`, you should see a graphical frame with a
    white background appear. Success!


<a id="org52315c4"></a>

### Windows

1.  Required Software

    -   Git for Windows: <https://git-scm.com/download/windows>
    -   Rustup: <https://rustup.rs>
    -   Microsoft C++ Build Tools: <https://visualstudio.microsoft.com/visual-cpp-build-tools/>
    -   CMake: <https://cmake.org/download/>
    -   Python 3: <https://www.python.org/downloads/windows/>
    -   Ninja: <https://github.com/ninja-build/ninja/releases>

2.  Building

    Run the downloaded executable files for all the above programs except
    for Ninja, which does not have an installer. If the installers inquire
    about changing the PATH, approve the changes.
    
    Extract the Ninja archive into `C:/Ninja` or a directory of your
    choice, then add that directory to the system PATH. This can be done
    through the Advanced System Settings dialog.
    
    Open Git Bash, the shell emulator that comes with Git for
    Windows. Navigate to an appropriate directory and run `git clone
    https://github.com/stark-cad/stark.git`. Alternatively, you can clone
    via SSH.
    
    Assuming that you will use DirectX 12 as the graphics API, run
    `/tools/set-backend.sh dx12` from the repository's base directory. If
    your system does not support DX12, you need a driver update.
    
    Run `rustup update` to ensure that an up-to-date Rust toolchain is
    present. Compile STARK by running `cargo build`.
    
    When you run `cargo run`, you should see a graphical frame with a
    white background appear. Success!


<a id="org12929ab"></a>

### Other

The core components required to build STARK are Git, Rust, `shaderc`,
and a supported graphics API. If you have access to another type of
system and want to try building STARK, try it and let me know how it
goes.


<a id="org654ec54"></a>

## Usage

There are three main modes available to run STARK in, specified by
command line arguments. With no arguments (`cargo run`), the
application runs in the standard graphical mode. With a single
argument (`cargo run repl`), only a Sail REPL at the command line
runs. With two arguments (`cargo file examples/mult-while.sl`), the
second argument is taken as the path to a Sail file, which is
executed.

Here we will discuss use of the graphical mode; more information about
Sail is given in the next section. Currently STARK presents a simple
canvas occupying the entire frame. You can alter it directly by
drawing lines with your mouse and keyboard, or indirectly by running
functions at the provided REPL.

Inside the frame, your cursor is a crosshair. Click once to begin
drawing a line; a preview will appear. Move the cursor and click again
to place the second point, completing the line. You can draw as many
lines as you like in this way.

Keyboard controls are also provided for more precise diagramming and
situations when you lack a mouse. The current keybindings are as
follows.

-   **Space**: Place a point here
-   **U**: Move cursor up
-   **D**: Move cursor down
-   **F**: Move cursor right ("forward")
-   **B**: Move cursor left ("back")
-   **L**: Increase step length ("lengthen")
-   **S**: Decrease step length ("shorten")
-   **E**: Escape a line in progress
-   **K**: Kill the last drawn line
-   **M**: Switch drawing modes

These are all hardcoded at the moment, and selected to be agnostic
with respect to keyboard layout, but users will soon be free to rebind
all these functions at runtime.

As the final binding indicates, there are two drawing modes. The
default is drawing a line segment between two points. When you switch
modes, however, placing two points will draw a rectangle instead.

Certain functions are available at the REPL for changing the graphical
state (caution: for now, entering an invalid function crashes the
program):

-   `(clear-lines)`: Clears all lines currently on canvas
-   `(back-col-set r g b)` Takes three float color values between 0.0
    and 1.0; sets the background color of the canvas
-   `(line-col-set r g b)` Takes three float color values between 0.0
    and 1.0; sets the color for all lines until it is changed
-   `(draw x1 y1 x2 y2)` Takes four float coordinates between -1.0 and 1.0,
    specifying two points; adds a line or rectangle to the canvas
    according to the current mode


<a id="orga517d1e"></a>

## Sail

Sail is a scripting language used to define most of STARK's
behavior. Since it is interpreted, the code underlying STARK can be
altered while the program is running. This gives the system a great
deal of malleability: a user can change the software as easily as use
it.

A Lisp dialect, Sail takes cues from Common Lisp, Clojure, and
Scheme. The language is unique, and it will grow and change with STARK
as a whole, offering great power tuned to the needs of computer
augmented design.

There are several basic data types in Sail:

-   Integers: `42`
-   Floats: `4.2`
-   Bools: `#T`; `#F`
-   Lists: `(1 2 3)`
-   Vectors: `[0.6 :keyword (2 4)]`
-   Strings: `"sail"`
-   Maps: `{ :one 1 :two 2.0 }`
-   Procedures: `(fn [a] a)`

Here is an example procedure definition and use:

    (def add (fn [a b] (- a (- 0 b))))
    
    (def acc 0)
    (set acc (add acc 1))
    (set acc (add acc 2))
    
    ; acc is now set to 3

More example lines:

    (if #T :yes :no)
    ; evaluates to :yes
    
    (def count 0)
    (while (not (= count 10)) (set count (+ count 1)))
    (print count)
    ; prints 10
    
    (print (do (print 1) (print 2) 3))
    ; prints 1, 2, 3 on separate lines
    
    '(+ 2 2)
    ; evaluates to (+ 2 2)
    
    (eval (parse "(+ 2 2)"))
    ; evaluates to 4

Sail currently uses an iterative stack-based evaluator which walks a
structure of Sail objects in memory. It evaluates lists containing
special forms or functions along with their arguments. There are many
improvements to be made, so, like the rest of STARK, this language
will change at all levels as development continues.


<a id="org531775c"></a>

## Roadmap

STARK has a long way to go. Some upcoming improvements:

-   Text rendering
-   Move REPL into graphical frame
-   Save / load text and diagrams
-   Improved document data storage
-   Edit multiple items at once
-   3D chunk rendering


<a id="org5eec16c"></a>

## License

STARK is licensed under the terms of the GNU Affero General Public
License. See the LICENSE file for the license text.

Find full copyright information in the COPYRIGHT file.

