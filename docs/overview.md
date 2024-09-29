
# Table of Contents

1.  [STARK](#org3fe822d)
    1.  [Explanation](#org0301cf9)
    2.  [Files](#org47c032f)
        1.  [`Cargo.toml`](#org124757d)
        2.  [`Makefile`](#orgbff8dd4)
        3.  [`examples`](#org6f6a81a)
        4.  [`scripts`](#org97473f3)
        5.  [`src`](#org986b8d9)
        6.  [`tools`](#orgb131333)

<!--
STARK, a system for computer augmented design.

SPDX-FileCopyrightText: © 2021 Matthew Rothlisberger
SPDX-License-Identifier: CC-BY-SA-4.0

STARK documentation is licensed under the terms of the Creative
Commons Attribution-ShareAlike 4.0 International license. See the
top-level LICENSES directory for the license text.

The STARK name and all associated trademarks are property of Matthew
Rothlisberger. Separate limitations apply to any use of these
trademarks. See the Trademark Permissions file for usage details.

Find code copyright information in the top-level COPYRIGHT file.
-->


<a id="org3fe822d"></a>

# STARK

This overview applies to v0.1.0 of STARK.


<a id="org0301cf9"></a>

## Explanation

A prototype of a malleable tool for computer augmented design.


<a id="org47c032f"></a>

## Files

All the files I have written for STARK are in this repository; I may
move some dependencies in as Git submodules in the future.


<a id="org124757d"></a>

### `Cargo.toml`

This file configures Cargo, the Rust build system, for this
project. It gives the package name, current version, and a listing of
libraries that STARK depends on. Cargo uses this listing to compile
and link the resulting program with all the necessary code. At the end
of the file is an entry describing the graphics backend dependency,
which may vary between compilation targets. This can be altered using
a shell script included in the repository.


<a id="orgbff8dd4"></a>

### `Makefile`

For use with GNU Make, this file is meant to contain command sequences
I run frequently, and abbreviate them to a simple `make`
command. Currently it contains only a recipe to document STARK's Rust
code and copy the documentation pages to the STARK website source.


<a id="org6f6a81a"></a>

### `examples`

Sail example programs go in this folder; for the moment there are only
two. They compare two different iteration strategies that Sail
supports.

1.  `mult-tail.sl`

    This program calculates the sum of all the multiples of 3 or 5 up to
    1000 (not inclusive). It uses tail recursion, which highlights a
    current failing with the Sail interpreter: tail recursion is much
    slower than regular iteration. This is because of the way the
    environment is built up, and will be rectified in the future.

2.  `mult-while.sl`

    This has exactly the same behavior as the program mentioned above, but
    uses a while loop instead of tail recursion, and consequently is much
    faster. These programs also indicate the utility of certain
    capabilities, like macros and good garbage collection, which will be
    added to Sail shortly.


<a id="org97473f3"></a>

### `scripts`

In this folder are Sail scripts that are important to STARK; they
actually describe its behavior in the main graphical mode. The current
two are automatically run by the main thread and the render thread,
respectively, and communicate through queues.

1.  `main.sl`

    This script defines the behavior STARK exhibits upon receiving various
    user inputs. There are assorted convenience functions, followed by a
    main loop which checks a queue for user input. Signals are sent along
    this queue from the context thread, which directly receives input from
    the operating system. There are several keybindings, most of which
    move the cursor around the canvas. The mode may be switched between
    drawing lines and rectangles; the cursor step length may be modified;
    and other keybindings change the drawing state, such as canceling or
    deleting lines. Several of these behaviors involve sending signals
    along another queue, to the rendering thread, where the graphics
    engine state changes.
    
    A couple of items are defined before the main function, because they
    are used across threads. That kind of simplification is possible
    because only one thread ever writes to these Sail objects. This script
    also handles input from the Sail read evaluate print loop. Another
    thread waits for input on the standard input stream, sending it to the
    main thread over the queue. When this input arrives, the main function
    parses and evaluates it, printing the result on standard output; this
    permits STARK to run arbitrary code, provided by the user, at
    runtime. A few of the functions defined in this script, such as the
    one that changes the color of subsequently drawn lines, are expressly
    for users to run at the REPL.

2.  `rndr.sl`

    This program runs in the rendering engine's thread. It receives draw
    calls from the manager thread describing geometry to add to, or remove
    from, the canvas. It also receives information from the context thread
    on changes to the graphical frame that require action by the
    engine. When new information arrives, Rust native functions run to
    alter the graphics engine. The tracking line that shows the
    prospective position of the next line once one point has been placed,
    is currently also drawn here. This capability uses global data that is
    written only from the manager thread, but it is likely to be moved
    entirely to that thread in the future. The rendering engine's
    interface to the rest of the program is in an early state.


<a id="org986b8d9"></a>

### `src`

This directory contains all of STARK's Rust source code, as well as
some GLSL graphics code. The core components of the program, the
graphics engine and the Sail interpreter, are implemented in
Rust. Many low level Sail functions are also written in Rust.

1.  `graphics`

    This is the rendering engine module; it defines a graphics engine and
    the main loop for a thread specific to rendering. STARK essentially
    uses the Vulkan API, through a Rust library that permits compilation
    for this or several other graphics APIs. Another directory here
    includes the shaders the engine uses.
    
    1.  `shaders`
    
        Shaders are programs that are compiled for, and run on, a computer's
        graphics processor. They define the behavior of particular stages of a
        rendering pipeline. There are only two shaders here at the moment,
        though more will be added in the future. The current ones simply work
        to draw colored lines.
        
        1.  `lines.frag`
        
            The line fragment shader is used during rasterization to determine
            drawn pixel colors. Here, color information is accepted from the main
            program as a push constant, a value which will remain the same for
            every time this shader is run to complete a draw call. The shader is
            run for every pixel in a drawn line, so the color will be uniform
            across the line.
        
        2.  `lines.vert`
        
            The line vertex shader takes vertex information from the main program
            and outputs vertex positions in the graphics processor's internal
            format. In this case, it simply takes a pair of X and Y coordinates
            for each line vertex and outputs the corresponding four part
            coordinate used by the rest of the graphics pipeline.
    
    2.  `mod.rs`
    
        All the Rust rendering code is in this file. It sets up a collection
        of important graphics objects from the `gfx-hal` library, which is a
        minimal abstraction over the Vulkan API. To initialize the engine,
        first the interfaces to the window manager and the graphics processor
        are set up. Then a structure describing the details of the desired
        render pipeline, including the aforementioned shaders, is
        created. Memory for buffers, which store vertex data for graphics
        processor use, is acquired. After the engine setup is finished, the
        clear color is set to a default white and the render thread script
        (`rndr.sl` from earlier) is started.
        
        This script has access to a set of Rust native functions that provide
        an interface to the graphics engine from Sail. It can redraw the
        image, signal a change to the frame size, add a line by vertices and
        color, remove a line from the list, clear all the lines, or set the
        background color. The line vertices and colors are stored in two
        vectors (dynamic arrays) of fixed arrays. Space needed for the line
        list is tracked, and the size of the graphics processor visible buffer
        is increased if necessary. Adding and removing lines are simple
        operations on the vectors of vertices and colors.
        
        To draw a single frame, all graphics processor synchronization
        constructs are reset and an image to draw to, which will appear on the
        graphical frame, is acquired. The contents of the vertex list are
        written into the vertex buffer and made visible to the graphics
        processor. A buffer of commands to submit to the graphics processor is
        recorded; this draws each vertex in order while advancing to the next
        color for every line, meaning every other vertex. On each draw, the
        entire image is updated. After the command buffer is fully recorded,
        it is submitted to the graphics processor for execution, then
        presented on the graphical frame when rendering completes.
        
        Errors can occur during the draw process, usually when the current
        graphical frame size does not match the graphics engine's knowledge of
        the frame size. This makes it impossible to acquire or present an
        image, so when these errors occur, the draw function sets a flag and
        returns immediately. This flag indicates that the swapchain, which is
        the engine's connection to the system window manager, must be
        reconfigured. The flag is checked regularly; when it is set, a
        function runs that resets the swapchain according to the new frame
        size, which is automatically updated as often as possible by the
        render thread script.

2.  `sail`

    Sail is the scripting language built into STARK, which enables its
    behavior to be modified even as it runs. It is a unique Lisp dialect
    which, currently, is executed using a stack based treewalk
    interpreter. Sail is still rather simplistic, but it is already at the
    core of STARK and features many core types, branching control flow,
    loops, first class procedures, lock free queues for multithreaded
    operation, a small standard library, and more. In the near future, I
    intend to add powerful type and module systems, provide better
    debugging tools, extend the standard library, and add macros.
    
    Some interesting technical aspects of Sail are its recursion free
    interpreter, its symbol interning system, its unique value
    representation, its custom memory allocator, and its queue system
    mentioned above. Further optimizations and enhancements will include a
    fast garbage collector, an environment layout with good cache
    locality, a multiple precision number system, and a bytecode
    compiler. Sail is closely connected to STARK; I expect that in the
    future, most of STARK's code will be written in Sail, so it is
    important for this code to execute as quickly as possible.
    
    1.  `core.rs`
    
        This file contains the Sail core types and their associated functions,
        as well as the definitions required to set up a Sail environment and
        symbol table. The core types are nil, booleans, numbers in a variety
        of formats and bit widths, symbols, references, error codes, vectors
        for various purposes, and procedures. Different types vary in purpose,
        format, or both. One of Sail's primary architectural issues for now is
        the lack of a full type system; programmers cannot define their own
        types. The core types are suitable for simple systems, but a much
        improved type system is a priority.
        
        Sail objects can vary widely in size and layout, according to their
        type. If the values were simply stored with no additional data, it
        would be impossible to discern the boundaries between objects or their
        contained values. To prevent this issue, all Sail objects begin with
        an eight byte header of the same format. The header specifies the size
        and type, provides a reference count for garbage collection, and
        contains a pointer to the next list element. Since an object's type is
        always given in a known format at its start, Sail objects that vary in
        size and content can be safely handled through pointers, with all
        value accesses conforming to the type layout.
        
        All Sail objects contain a pointer for a subsequent list element. This
        is a departure from most Lisp dialects, which have a distinct cons
        cell type containing two pointers to other objects. Lists are then
        constructed from chains of cons cells, which leads to a great deal of
        indirection; many objects that consist only of pointers must be
        traversed to reach list elements. In Sail, every object may be thought
        of as a cons cell with one value stored immediately and one pointer to
        the next cell. This includes list structure information in every
        object and eliminates the concept of malformed lists.
        
        This design decision is central to the language because it affects
        every object and lists themselves, which are at the core of every Lisp
        dialect. I have debated this design choice extensively, especially
        against the standard Lisp layout with cons cells. I may still change
        the layout in the future, but I think that including a pointer in
        every object has several benefits. By making all objects at least
        eight bytes in size, the future design of the garbage collector is
        simplified. Embedding the list structure in the list elements reduces
        the time required to traverse a list, reduces memory use, and improves
        cache locality.
        
        With such a layout, though, an object must be copied into another
        location to be used in a list structure besides the one in which it
        was created. There are many ways to optimize the behavior of copies
        and writes, especially when a compiler will be used. I am yet to
        devise tests of this, but I believe that the positive qualities of
        this format outweigh those of the classic Lisp list format. They seem
        to be equivalent in what they can express, so Sail is quite likely to
        feel similar to other Lisps. Nevertheless, I am interested to discover
        whether improvements to syntax or behavior can stem from the unique
        layout.
        
        A portion of this core file is devoted to utility functions for
        handling Sail objects: type testing, truth testing, retrieving size,
        extracting header bytes, getting the next list element pointer,
        finding the start of the held value, and more. Following this are many
        functions for making default objects of core types, and for
        initializing them with a value. These latter functions can use several
        optimizations compared to simply writing to default objects. Compound
        types, like procedures and vectors, have several other functions for
        extracting the values stored within. In compound types, the values are
        packed following the header.
        
        There are functions for equality tests and hashing. The equality test
        is somewhat convoluted to handle comparing nil values to empty
        references; this points to a need for improvement in the way these
        types work together. Equality tests and hashing are important to hash
        maps, which are used to implement the Sail environment. This is the
        structure that relates unique symbols to objects. Symbols are
        essential to Lisp systems: they provide values that are accessible by
        name even at runtime. My current environment implementation is naive;
        it takes too many steps to look up symbols and the data is not closely
        packed. I will improve this, but it works for now.
        
        Using the current design, a Sail environment is a list of associative
        maps. Each map relates keys to values: the keys are always symbols and
        the values can be any Sail object. The list structure creates a
        hierarchy of environments; they are searched in order so that entries
        in more recent maps take precedence over their parents. The maps may
        be hash tables or simple association lists. Since these hash tables
        use linear probing to handle collisions, an association list may be
        thought of as a hash table with only one bucket to sort into. To
        dereference a symbol, the environment is searched until the symbol's
        entry is found, and the referred object is returned.
        
        Along with the environment, Sail relies on a structure called a symbol
        table. It is essential for parsing user programs and for printing Sail
        structures in a legible way. This necessity arises from the fact that
        a symbol in textual Sail code may be nearly any string of ASCII
        letters and digits. Sail structures in memory, however, represent
        symbols as unique 32 bit values: symbol IDs. In this way, all symbols
        have the same defined size, but the maximum number of symbols is
        limited to around one billion (currently there are four types of
        symbols; this tag occupies the high two bits of the ID). The symbol
        table relates the text strings that represent symbols in code to the
        IDs that represent symbols in memory.
        
        A symbol table is a vector containing two associative maps and a dummy
        symbol which acts as a counter. The maps are hash tables that always
        contain the same set of entries. The first map is indexed by symbol
        IDs (no hashing is needed because these are already 32 bit numbers);
        the second map is indexed by the hashes of symbol strings. Each cell
        the table buckets point to is a pair containing a symbol (ID) and a
        string. Since there are two maps in this "bimap" configuration, the
        set of entries can be efficiently searched for an ID, returning a
        string, or for a string, returning an ID. Every time a symbol is added
        to the table, the counter symbol is incremented, such that it always
        stores the next available symbol ID.
        
        There are functions to create a symbol table; to insert a symbol; and to
        lookup using a symbol object, a string object, or a bare symbol
        ID. A function that looks up by a bare string slice and inserts an
        entry for it if not found, thus returning a valid ID for any input
        string, is applicable throughout the parser. On the other end, when a
        Sail structure from memory must be printed out for the user, all the
        symbol objects are looked up in the table by their IDs and the
        resulting strings are substituted in the output. The symbol table is
        not strictly necessary to execute Sail code that already exists as a
        structure in memory, but it is required to interface with a user.
    
    2.  `eval.rs`
    
        Sail programs are parsed from text into a structure of Sail objects in
        memory. This file contains the mechanisms required to evaluate such
        structures and return the resulting objects. The evaluation system has
        changed over time perhaps more than any other aspect of Sail. As it is
        now, evaluation uses a custom stack structure that executes frames
        each containing a return address, an environment, an opcode describing
        the nature of the computation, and one or more pointers to Sail
        objects. Based on the opcode, the evaluator uses the Sail objects and
        the environment to produce a result, a pointer to which is placed at
        the return address.
        
        A Sail stack is a contiguous region of memory tracked with a group of
        pointers: the start of the stack, the end of the stack's available
        space, the current top of the stack, and the start of the stack's top
        frame. There is also a pointer to an otherwise unused memory location
        where unnecessary values are returned. To create the stack, some
        number of 64 bit machine words are allocated and the pointers are
        set. The stack top and the frame start are simply the start of the
        stack's memory since no frames exist yet. A stack can also be resized,
        which may allocate new memory. If this occurs, any pointers to memory
        within the stack are modified for consistency with the new location.
        
        Similar to any stack, the simple push and pop operations are
        present. Pushing a value, which in this case is always a pointer to a
        Sail object, increments the stack top pointer and writes the value to
        the newly available space. It also checks whether the stack needs to
        be resized, which adds some operations; including optional fixed size
        stacks may be prudent. Popping a value does nothing but decrement the
        stack top pointer. The memory will be overwritten by the next push, so
        it does not need to be altered. Pushing a value makes it part of the
        top frame, or current frame. Adding a new frame to the stack uses a
        function that pushes a frame head.
        
        All stack frames start with a frame head. This is three words long,
        always of a standard format, and built according to certain rules. The
        first word in the head is a pointer to the start of the last frame;
        this forms a chain of pointers from each frame to the previous
        one. The bottom frame points to itself, which is also the start of the
        stack. These pointers are included so that, when a frame is popped,
        the position of the previous frame may be recovered and placed in the
        frame start pointer. It is unnecessary to have immediate access to any
        frame except the top one, but if the stack lost track of any frame
        positions, execution would rapidly fall apart.
        
        The second word of a frame head is the return address. Every stack
        frame, when executed, returns a Sail object. The return address points
        to the location where the pointer to that object will be written. Most
        return addresses point to locations within a previous stack frame. The
        stack's utility, then, is that execution can be broken down step by
        step, spawning new frames which work to fill the slots in older
        frames. A stack of frames builds up as the program's complexity is
        broken down, then once indivisible units are reached, the stack
        collapses back down, each frame filling a slot below itself with the
        results of its small computation.
        
        A frame head's final word is a tagged pointer containing two important
        pieces of information. The first is a pointer to an environment. As
        previously discussed, an environment is essential to Sail
        execution. The environment in the frame head tells the evaluator where
        to look up symbols encountered while executing a particular
        frame. Since pointers on x86-64 systems are only 48 bits long, there
        are two extra bytes of space. One of these is used to store an opcode,
        which is a number that determines how the frame will be treated by the
        evaluator. Different opcodes mean different behavior, and different
        requirements about the length and content of the frame body.
        
        Popping an entire frame off the stack is similar to popping a single
        word. The stack top pointer is set to one word before the start of the
        current frame, and the frame start pointer is set to the same address
        as the previous frame pointer from the frame being popped. Once this
        is done, the frame being popped cannot be accessed and its previous
        frame is the new current frame. Again, the space previously occupied
        by the popped frame will simply be overwritten by new pushes as
        needed. There is an unused function to unwind the stack, popping
        frames until the stack start or another point of interest. This will
        probably be used for error handling in the future.
        
        There are utility functions to determine whether the stack is empty;
        to give the current frame's return address, environment, and opcode;
        to give the address of an offset into the current frame's body; and to
        give the Sail object pointer stored at an offset into the current
        frame's body. Another important function evaluates any Sail expression
        (in object form), adding a new frame to the stack if necessary. This
        function is used to start evaluation on the stack, and sees frequent
        use within the evaluation logic. It takes a return location, an
        environment, and a pointer to an expression. If the expression is not
        a list it can be evaluated and returned without pushing a frame.
        
        Expressions that are lists must be evaluated using stack frames so
        that they can be broken down (pushing frames) and evaluated piece by
        piece (returning values and popping frames). The most important Sail
        stack function consumes the stack's top frame and executes it
        according to logic specific to its opcode, spawning new frames if
        necessary. This is a single step, or iteration, of evaluation. This
        function only operates on one stack frame at a time, does not use any
        recursion, and, barring a crash, always returns. Fully evaluating any
        Sail expression requires starting evaluation on a stack, then
        repeatedly iterating execution until the stack is empty.
        
        Avoiding recursion in this design is important. Previous Sail
        evaluator schemes used recursive descent, in which the evaluation
        function calls itself with new target expressions in order to break a
        computation down into small pieces. This can create a chain hundreds
        or thousands of recursive calls deep. These calls use the call stack
        provided by the operating system, which has an unchanging size. Issues
        arise when Sail evaluation fills the stack, triggering an overflow and
        crash. The standard call stack is also a poor fit for Sail execution,
        because frames are created and destroyed inefficiently and must
        contain far more data than is necessary.
        
        The current Sail evaluator, with its customized stack model, takes
        less memory overall and always uses a small, bounded number of frames
        on the application call stack. The Sail stack, stored in allocated
        heap memory, is able to grow as necessary. It also uses a format and
        algorithm optimized for the needs of Sail. Switching to this evaluator
        from the old recursive design led to well over tenfold improvements in
        test program run time. This model has enormous potential for new
        capabilities, particularly error handling logic and debugging
        tools. Future Sail evaluation models, including a forthcoming bytecode
        interpreter, will take cues from this design as well.
        
        An iteration of stack evaluation begins by extracting the return
        address, environment, and opcode from the top frame. Then the function
        branches on the opcode value to select which operation to
        perform. Currently there are nine opcodes. The pre evaluation opcode
        indicates that the frame body has one pointer to any valid Sail
        expression. If the expression is not a list, the result is returned
        immediately; if it is a list, a replacement evaluation frame
        spawns. This opcode is essential for the `eval` operator, which
        evaluates Sail structures within Sail code. Its argument is evaluated,
        and then that result, whatever it may be, must be evaluated.
        
        List evaluation is the core of Lisp, so the same is true of Sail. The
        most common opcode serves this purpose. An evaluation frame's body
        contains one pointer, to the first element of a list. An operator,
        which may be a procedure or special operator, must always be the
        list's first element. A number of arguments may follow as the
        subsequent elements. The operator is checked to discover whether it is
        one of the special operators. These each require unique logic that
        prevents them from being implemented as procedures. If the operator is
        not special, it must either be a symbol referencing a procedure or
        another expression that evaluates to a procedure.
        
        There are nine Sail special operators. `def` binds an object (the
        result of evaluating its single argument) to a symbol in the
        environment. `do` evaluates any number of argument expressions in
        order, returning the result of the last one. `eval` evaluates its
        argument, then evaluates that result. `fn` constructs a new procedure
        from an argument list and a body. `if` takes a predicate and two
        branches: the first is taken if the predicate result represents truth;
        the second is taken if it represents falsity. `quote` returns its
        argument without evaluating it. `set` is the same as `def` except that
        it fails if the symbol is not already bound. `while` evaluates its body
        repeatedly until its predicate evaluates to a false value.
        
        Each of these special forms, when encountered during evaluation,
        either returns immediately or causes a maximum of two frames to be
        pushed onto the stack. If any frames are pushed, one of them replaces
        the evaluation frame, leading to a more efficient use of stack
        space. When the operator is not special, it must evaluate to a
        procedure, which will be applied to the arguments comprising the rest
        of the list. If the operator is itself a list, it must be evaluated
        before procedure application, so a frame is spawned to resolve the
        arguments later, with an evaluation frame spawned above it to return
        the procedure beforehand.
        
        If the procedure can be immediately resolved from a symbol, argument
        resolution begins right away. A frame is pushed that will apply the
        function once all the arguments have been evaluated and returned. As
        many evaluation frames as necessary are then pushed to resolve the
        arguments and return them prior to application. Precise function
        application logic, similar to the rest of the evaluation system, has
        been in flux since the beginning of Sail. It has been improving
        through time, but it is still subject to a great deal of change and
        optimization. Part of the difficulty comes from interfacing my own
        language (Sail) with the language (Rust) I am writing STARK in.
        
        The next opcode binds a given symbol to a given object in the
        environment's top level. This inserts a new entry, whether or not a
        previous entry for that symbol exists. The subsequent opcode mutates
        an existing entry by changing the object it points to. Using mutation
        for symbols that are already bound is much more efficient than
        creating new entries every time a variable's value changes. When
        mutating, in the current design, the entire program crashes if no
        entry exists for the given symbol. Of course such a failure is
        unacceptable and will be rectified with a broader error handling
        system in the near future.
        
        The opcode following these tracks a sequence of expressions to be
        evaluated in order. The sequence frame persists on the stack until the
        last expression is reached; then it is replaced with an evaluation
        frame whose result is returned. In this way, only the final
        expression's result is written to the sequence frame's return
        location. The next opcode, which establishes a while loop, uses this
        sequencing capability. A while loop checks whether a predicate is
        true, and if so, executes a sequence of body expressions. These steps
        repeat until the predicate is found to be false. The while loop frame
        is not destroyed until this point, and always returns nil.
        
        An even more essential control flow opcode provides branching. The
        frame takes a predicate and chooses one of two paths, based on its
        truth value, with whose evaluation it will be replaced. This
        capability is accessed with the `if` special operator. The next opcode
        denotes a pre application frame for procedures. These frames provide
        delayed argument resolution for cases where an expression must be
        evaluated to acquire a procedure. A procedure application frame, the
        final kind, takes a procedure and an appropriate number of resolved
        arguments. It then applies the procedure, which may be implemented in
        Sail or Rust, and returns the result.
        
        Procedure application is an integral aspect of the evaluator, as it
        permits code reuse. The application frame logic checks whether the
        given procedure is written in Sail or in Rust, as these have different
        calling conventions. If it is written in Sail, the argument symbols
        and the arguments themselves are added to an inefficient structure
        called an environment argument layer, which is added to the top of the
        environment before sending the procedure body to a sequence frame for
        evaluation. For procedures written in Rust, the arguments are taken as
        a slice directly from the stack frame, the function is executed, and
        the result is returned immediately.
    
    3.  `memmgt.rs`
    
        All Sail objects occupy space in heap memory; this file contains the
        memory management functions. Sail memory is divided up in multiple
        ways: into regions, zones, and blocks. A region is the broadest
        division, made up of a linked list of equally sized zones. A zone can
        be filled with blocks of variable size, each of which corresponds to
        one Sail object. Zones are allocated into regions from the system,
        while blocks are allocated from a region and may lie within any of its
        zones. Every Sail object occupies its own block, within a zone, within
        a region. Computations on objects in different regions are guaranteed
        to be fully independent unless queues pass between them.
        
        A persistent structure, the region table, uses parallel arrays to
        track the starting and ending addresses of every allocated zone, as
        well as the region to which each belongs. Since objects are always
        handled using pointers, any object's region and zone may be discerned
        using its address and the region table. The structure of the region
        table has been through several iterations. The current design is a
        completely custom set of four parallel arrays that resize together as
        necessary, with length and capacity tracked. There is also a write
        lock to make sure that thread contention does not invalidate the
        contents of the table.
        
        The central memory management function allocates a given number of
        bytes from a given region, with a given configuration byte for the
        block. The total block size will be larger than the given size, as
        there must be room for the Sail object header. The head size ranges
        from eight to sixteen bytes, and is determined by some of the
        configuration byte contents. An available zone is locked while a new
        block is allocated, then the configuration is written into the first
        byte and the pointer to the newly created object is returned. The
        object is also given an initial reference count of one. The reference
        count will be important to the memory manager in the future.
        
        Zones begin with a structure that tracks the number of used bytes, a
        pointer to the end of the used portion, a pointer to the start of an
        internal freelist (for future use), a pointer to the next zone in the
        region, and a lock that prevents multiple threads from attempting to
        allocate objects simultaneously. Since a region is made up of multiple
        fixed size zones, it can grow to any size without costly reallocation
        copies. Following the zone head structure is a span of memory into
        which blocks can be allocated. Blocks, of course, can vary wildly in
        size; the only requirement is that each object's size be discernible
        from the first few bytes.
    
    4.  `mod.rs`
    
        This file contains general functions and other definitions for
        Sail. The contents of all the other Sail files started out in this
        file before being split out. The current version begins with various
        utility functions for dealing with Sail error codes, which correspond
        to the elements of an enumeration of errors in Rust. This error code
        system bridges Rust and Sail, and is a first error handling
        prototype. Handling errors and other conditions, without crashing the
        entire program, is one of the major areas slated for improvements in
        the near future. This will likely require integration through various
        parts of the Sail system, especially the evaluator logic.
        
        There is also implementation for fixed size arrays that store values
        directly. These may be created with an unchanging length and a sized
        base type to store. They are more efficient than standard vectors for
        collections of values with like types. In particular, they have found
        a use in places where sets of related values are overwritten
        frequently, such as cursor position or coordinates to draw at. Similar
        to the error codes, a grouping type that works for any sized type is
        an early prototype for a more flexible Sail type system. For the
        moment all types are predefined, but a programmer should be able to
        create and use their own types efficiently in the future.
        
        An important aspect of this file is the collection of Sail symbols
        that the Sail runtime needs to be aware of at a low level. These few
        dozen symbols are defined with specific symbol IDs and names, so that
        they can be efficiently referenced from Sail or Rust code. They also
        each have a defined symbol type, which for now indicates whether they
        are basic symbols, keywords, or denote Sail types. These symbols
        include those for basic Sail types, every special operator, along with
        important names and keywords for the STARK system. It took some time
        to work out how to make Sail symbols available to the low level Rust
        code, and the method may change.
        
        Some functions here, mostly unused, deal with object type specifiers
        that are part of enhanced type system prototyping. These type
        specifiers would be particular type symbols, and reference some kind
        of entry describing the type. More design is necessary, and there are
        trade offs to be made at various levels. Following this are functions
        that provide a hacked together way of displaying Sail objects and
        structures as text. This kludge has lasted since the beginning of
        Sail: it's inefficient and quite poorly integrated, but it gets the
        job done for now. Display is also on the list of Sail subsystems in
        need of revitalization.
        
        There are functions to start a Sail REPL and run a Sail file, which
        tie everything else together. They are interpreters that take in
        textual Sail code and output a textual Sail object representation,
        performing all the parsing, evaluation, and final display in
        between. These rely on functions that set up the environment,
        inserting all of the important symbols mentioned above, as well as the
        library of native Rust procedures. Such procedures must have a
        specific signature and must be defined in a particular format. A macro
        from another file creates a slice of definitions in this format, which
        are added to the environment for use at runtime.
    
    5.  `parser.rs`
    
        Like nearly every other programming language, Sail has a textual form
        for people to write and read. The parser, described in this file,
        reads the text of Sail code and builds up a corresponding Sail object
        structure for evaluation. It uses a recursive descent algorithm, in
        which a collection of functions, each for parsing a different part of
        the grammar, call each other. Thus the system stack is used to track
        chains of nested elements, usually lists. Function frames build up
        until the innermost part of the program is reached, then collapse back
        down, returning one by one. The parser will not crash, instead
        returning error codes for invalid input.
        
        All of the parsing functions that are part of the recursive chain have
        the same signature. They take an iterator over the bytes of a string
        slice, with one character of lookahead; an accumulator to collect byte
        strings across multiple function invocations; a Sail memory region in
        which to allocate all the parsed objects; and a Sail symbol table from
        which to acquire symbol identifiers. They return a tagged enumeration
        that may contain either a pointer to the head of their output
        structure, or a Sail error code describing why parsing went wrong. In
        the future the parser will have an understanding of its position in
        the program and will be able to give the line numbers of errors.
        
        Parsing begins with a function that identifies the value to be read
        and calls the appropriate reader. It does this by examining the first
        character of the value. First it skips white space and comments prior
        to the first code character. After matching this character to those
        which indicate certain values, it calls the correct reader function
        with all its own arguments. When a character acts only to signal a
        particular type of value, it also consumes the character. After the
        reader has returned, the accumulator is cleared. An additional
        character of lookahead is used when an addition or subtraction sign is
        encountered, as these may be symbols or may be part of a number.
        
        The first reader handles the special syntax for quoted values. When a
        programmer wants to ensure that a value is not evaluated, instead of
        typing out a two element list with the quote special operator, they
        may simply prepend a single quote character. This reader builds
        the proper object structure for quoting: a list with the special
        operator followed by the value. In other Lisp dialects, this
        kind of syntax extension, in which certain characters are replaced
        with certain structures at parse time, is often called a reader
        macro. Sail has only this hard coded quoting syntax for now, but
        support for parser customization may be added.
        
        Perhaps the most used reader function is for lists, which are the core
        of all Lisp code. Lists are represented as space separated chains of
        values between parentheses. The reader tracks a list's head and tail,
        calling the value reader to get each element before appending it to
        the tail. Upon encountering the closing parenthesis, the head is
        returned to the calling function. Sail, unlike many Lisp dialects, has
        no syntax for malformed lists. Such syntax is useless because Sail
        lists are not made from standard cons cells. The Sail list structure
        is unique, consisting of objects chained together directly, each
        carrying a pointer to the next object or a nil pointer.
        
        There are two more basic Sail collection types with their own syntax:
        vectors, enclosed by brackets, and maps, enclosed by braces. Each of
        the three collection types (lists, vectors, and maps) has its own
        characteristics for insertion, deletion, and access. Since vectors and
        maps store many references to other objects within their single
        object, they are slightly easier to construct than lists. Each value
        is read and its returned pointer is added to the collection. Maps are
        unique since each entry consists of two elements, so the number of
        values in a map must always be even. Vector and map construction may
        be moved out of the parser eventually.
        
        Symbols are read using one of two similar functions, one for basic
        symbols and one for special symbols. The distinction exists for now
        because the selection of special characters available for use in basic
        symbols is broader than that permitted in special symbols like
        keywords or type identifiers. Symbols are simply read from the input
        stream character by character until a space is found. They are then
        inserted into the symbol table, and a symbol object with the correct
        ID is created and returned. Special symbols are preceded with a sigil
        character, and their IDs have one or both high bits set to indicate
        the symbol type.
        
        Strings and numbers are read in a similar way to symbols. Their
        characters are appended to the accumulator until a close quote or a
        space is encountered, then an object is created. Strings are
        initialized from the accumulator directly. Numbers have an additional
        parsing step. The Sail number system, like many other aspects, is in a
        prototype state. For now only 64 bit integers and floats can be
        entered in code. In time, the language is intended to possess an
        arbitrary precision number system capable of being mostly transparent
        to the user. It will be possible to enter numbers of great size, in
        multiple formats, with a custom radix.
        
        A final reader function handles special values, which are preceded
        with a pound sign. For now there are only two accepted special values:
        true and false. Some Lisp dialects simply use certain symbols as
        boolean values, but it seems clearer to have distinct true and false
        values available. The special value system will likely see expansion
        in the future; it could be useful for syntax extensions such as the
        reader macros mentioned above. The special value syntax may also be
        used for directives to the parser, evaluator, a future compiler, or
        other parts of the STARK system, to change their behavior in clearly
        delineated ways.
    
    6.  `queue.rs`
    
        Sail and STARK are designed from the beginning to use multiple threads
        on multiple processing cores in order to work faster. An essential way
        to facilitate cooperation between computational threads is by
        transferring data via queues. This file defines functions for sending
        and receiving Sail objects along a custom queue structure. Queues are
        created as pairs of a sender and a receiver, which may occupy
        different memory regions. Threads must use different evaluation
        stacks, and when they use different regions as well they are as
        independent as they can be. Threads connected only by queues have just
        one way to block each other.
        
        To create a queue, two memory region pointers are provided; these may
        be to the same region. Two Sail objects, a sender and a receiver, are
        then created. If the given pointers are to different regions, one
        allocation is made in each. The sender contains a pointer to the queue
        tail, which initially is the receiver, and to the region containing
        the receiver. The receiver contains a pointer to the queue head, which
        is initially nil, and to the sender. These links between the queue
        ends are used by the enqueue and dequeue algorithms to ensure
        consistent information passing between threads, without locks or other
        synchronization. Atomic operations from the x86-64 instruction set see
        extensive use to avoid undesirable overwrites.
        
        Sail objects can be sent along queues between threads. The queue
        transmit function takes two pointers: one to a queue sender, and one
        to any Sail object. The target Sail object is copied into the memory
        region of the sender's linked receiver. This copy operation currently
        copies only the target object, not any other objects it may point
        to. To send the item, several actions are performed in a loop. In the
        ideal case, this loop runs only once, appending the new item to the
        queue and exiting. Since the queue structure may be modified by
        multiple threads, the loop includes invariant checks. If these fail,
        the transmission is tried again.
        
        First the current list tail is retrieved from the sender. Its type is
        checked; if it is a queue receiver, it must be this sender's linked
        receiver, which is the head of the queue list. In both cases, the
        pointer to the next element is retrieved from the tail; this should be
        nil. A check determines whether the tail has changed since
        transmission began, and proceeds if not. If the pointer to the next
        element is not nil, the sender's tail pointer is not pointed to the
        actual tail, so it is advanced and the loop resets. If the pointer to
        the next element is nil, the pointer to the new element is written in
        its place. In this case, the element has been appended to the queue
        and is the new tail.
        
        Once the loop completes, the new element must have been successfully
        appended to the queue. The function finishes by attempting to change
        the sender's tail pointer to the newly added element. It is acceptable
        for certain writes to fail, as they will be corrected during future
        transmissions. This transmit function makes extensive use of atomic
        operations, particularly atomic compare exchange. This instruction
        causes a new value to be written to a memory location only if it
        currently contains a known value. The check and the optional write are
        completed such that no intermediate state is visible to other
        instructions. Checking state before changing it avoids invalid states.
        
        Objects sent along a queue are received at the other end. Like the
        transmit function, the receive function has a loop that, if all goes
        well, only executes once. From the receiver contents, pointers to the
        queue head, sender, and queue tail are found. After an invariant
        check, structure corrections are attempted if the queue is in an
        incorrect state. Once the queue head has been found, the receiver's
        head pointer is changed to point to the element subsequent to the old
        head. If the queue is empty following removal of the received item,
        the queue structure is set correctly for this situation. Once all is
        finished, the received object has its next list element field set to
        nil, and is returned from the function.
        
        This design is likely to change in the future, especially as the
        details of Sail threading and memory management become clearer. One
        issue is the inability to send list structures along a queue. Another
        is that the receiver returns nil if the queue is empty, not any kind
        of error or condition. It is unclear what the best choice here
        is. Additionally, the queue sender and receiver are unique types that
        are not covered by the current provisional Sail type system. There is
        an uncommon issue whereby the queue contains elements, but the
        receiver's queue head pointer is nil. This is handled as well as can
        be when it occurs, but there may be a way to prevent it
        entirely. Finally, the region system may change significantly.
    
    7.  `stdenv.rs`
    
        For Sail to be useful, procedures must be available in the default
        environment. This file contains definitions, written in Rust, for
        several procedures to use in interpreted Sail code. Each of these has
        been added out of necessity, and all the logic is temporary. I expect
        to overhaul the standard procedures and the Sail environment in the
        future. There is a Rust calling convention for functions usable from
        Sail; the interpreter expects every native Rust procedure in Sail code
        to have a certain signature. This consists of a pointer to a memory
        region, a symbol table, an environment, and a slice of pointers to
        arguments, which are Sail objects.
        
        There is a Rust macro to ease the writing of procedures with the
        appropriate signature. It reduces the code that must be repeated for
        every function, and collects many functions together into a structure
        that can easily be processed into a Sail environment. Every procedure
        here in the standard environment is given a name and an argument
        list. The Rust code comprising the procedure can use the arguments,
        and always returns a Sail object. Currently there are procedures for
        arithmetic, equality tests, logic, queue sending and receiving,
        collection management, output, and debugging. Besides this file, the
        same macro defines thread specific functions elsewhere.

3.  `context.rs`

    A graphical user interface must exist within a graphical context: a
    location to draw to. In the case of today's desktop operating systems,
    this is a frame, or window, managed by the system's window manager
    software. This file contains functions to create such a frame and get
    user input from it, using the `winit` library. The main context
    initialization function creates an event loop, which will receive user
    input from the window manager. Then a frame is established with the
    manager and linked to the event loop. The frame and event loop are
    returned, to be used for drawing graphics and getting input,
    respectively.
    
    The core context function is the input loop, which takes over the main
    thread and provides handling logic for the event loop. This logic will
    be run every time an event arrives from the window manager until the
    program ends. Before running the event loop, the input loop function
    spawns an additional thread, which takes lines of text input from the
    shell. These are sent to the interpreter thread and executed as Sail
    code. This mechanism permits code execution at runtime to modify the
    prototype's behavior. It is also likely temporary, as all input should
    come from the graphical frame.
    
    Within the event loop, `winit` code runs a handler, provided in this
    function, for each event. Almost every event prompts a message to be
    sent along a Sail queue to either the main interpreter thread or the
    rendering thread. Some events also mutate shared state, like the frame
    dimensions or the cursor position. The current message passing scheme
    is quite inefficient, as Sail objects are created in a region specific
    to the context thread, then copied by the queue logic. Since there is
    not yet a garbage collector, this contributes to general memory
    leakage along with the unnecessary copies. The arrangement works in
    the current prototype state, and it will be improved in time.

4.  `lib.rs`

    Here is STARK's main library file, the top level for all the modules
    that make it up. All these modules have been discussed, but there is
    still some useful code here that does not currently fit anywhere
    else. There is a type definition that refers to a `winit` window as a
    frame, following the Emacs convention. There is also a type for a
    frame handle, a wrapper around the `raw-window-handle` library's
    format. This is defined to permit sending the contained data to the
    graphics thread without complaint from the compiler.
    
    Central to this file is the manager loop, the core of the main Sail
    interpreter thread. The Sail script that defines almost all STARK
    behavior runs in this loop. It also sends commands to the window
    manager for actions like changing the cursor icon, visibility, or
    position. Some Sail procedures to test these capabilities are defined
    using the appropriate macro. To actually start the manager loop, the
    main script is parsed, an evaluation stack is created, the environment
    is loaded, and the main Sail procedure is applied.

5.  `main.rs`

    Every program must start somewhere; STARK starts with the main
    function defined in this file. In the default mode, it performs the
    top level initialization and spawns the child threads that contain the
    essential logic. When this is completed, it becomes the context
    thread, running the input loop. There are two additional modes for the
    program, which each act as Sail interpreters. A file containing Sail
    code may be parsed and run; this is useful for testing and
    benchmarking the language. A perpetual Sail REPL in the shell may be
    run instead; this is best for interactive language testing where the
    graphical systems are not necessary.
    
    In the default mode, the main function creates Sail memory regions for
    the main thread, render thread, and context thread. It creates a
    global symbol table, and Sail environments for the main thread and
    render thread. It creates a battery of Sail queue channels for
    communication between threads. It creates shared Sail objects for
    frequently changing values. After all this, the appropriate pointers
    are passed to the render loop and the manager loop as their respective
    threads are initialized. All setup completed, the input loop gets its
    own Sail pointers and the program begins to behave as designed while
    its constituent threads interact.


<a id="orgb131333"></a>

### `tools`

Tools for working with STARK code go in this folder. These will likely
be mostly for building and debugging. There is only one such tool at
this time.

1.  `g-api.sh`

    This small shell script modifies the Cargo configuration file to
    choose which graphics backend STARK will be compiled with. It takes
    one argument, a string representing an acceptable backend. It then
    uses a `sed` command to change the configuration. This tool will
    likely be removed soon, as I plan to replace `gfx-hal` with `ash`, a
    direct translation of the Vulkan API into Rust. Once this is done,
    STARK will exclusively use Vulkan as its graphics processing API and
    no platform specific changes will be necessary.

