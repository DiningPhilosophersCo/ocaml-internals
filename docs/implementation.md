---
title: Implementation
sidebar_position: 1
---

Delimited continuations are implemented with dynamically allocated
stack. The compiler runtime uses `malloc` to allocate space for a stack
and sets stack pointer to it everytime a `match_with` or `try_with` is
called. This introduces a new challenge: stack overflow checks.

Before OCaml 5, the runtime used system alloted stacks to manage stack
frames. With heap allocated stacks, the runtime has to insert stack
overflow checks in the function prologues (function prologues and
epilogues are assembly/low-level code emitted by a compiler at the
start and the end of a compiled function). But first, let's get some
basics out of the way.

All OCaml programs are just C programs linked with OCaml modules
OCaml is a high level language - none of OCaml values are exactly what they appear.
On 64-bit machines, integers are 63 bits wide ie. only 63-bit long
integers can be represents when a values is declared/inferred as
integer type. Other types, like records etc., that are allocated on
the heap are really just addresses in the heap with special bits for
the garbage collector. [The Real World OCaml](https://dev.realworldocaml.org/) has a great chapter on how
OCaml values are represented.

What creates all these values and operates on them?
The OCaml runtime is written in C. I find that [Ghuloum's tutorial on
compilers](http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf) is great introduction to understanding how programs
created with languages like OCaml work in the runtime. The tutorial
implements a lisp, but OCaml runtime is very similar in this regard.

Here's a high-level overview.

![img](/img/ocaml-runtime-high-level.png)

C program's `main()` calls a chain of functions that initialise a lot
of things - garbage collection, domain (threads) initialisation, CLI
argument array santisation and other utils. Domain initialisation also
initialises stacks and we revisit this topic later on.

After initialisation, C program calls an externally defined function
`caml_start_program`. This is defined in the assembly part of the
runtime and is architecture and operating system specific. Why in asm?
C is, after all a high level language. It doesn't let us manipulate
stack pointers and other registers. Being able to do this crucial as
we'll soon realise.

Why is `caml_program` - the entrypoint - created dynamically? Because
OCaml doesn't force programmer to define a entrypoint like C does. So,
programmers are free to define global (or you may call it "top-level")
expressions. caml<sub>program</sub> is a collection of all such global
expressions run together. Along with global expressions, it'll also
run some other initialisation that happens in the OCaml layer.


### Delimited Continuations in the runtime

In an implementation (no matter the language), a delimited
continuations are just a bunch of independent stacks which the
execution can switch as necessary.

1.  Stack initialisation

    Threads are called domains in OCaml because 'threads' itself is a very
    overloaded term. Each domain maintains some state - one of which is it's
    stack.
    
    As we learnt, we have two stacks: one for OCaml and one for C. Since
    they both share the same set of registers, the runtime also has to
    save-restore stack states to respect each languages calling
    conventions. You can see this in the `arm64.S` file of the
    runtime. Keeping this in mind, let's see how the OCaml stack is
    allocated and managed. The C stack is managed by the C compiler and
    isn't the focus of this post. We will however note how we prepare the
    registers before handing off the control to the C layer.
    
    It all starts with `domain.c` where a new domain gets created. Among
    other things in it's state, you find the stacks.
    
	```ocaml
        domain_state->current_stack = caml_alloc_main_stack(stack_wsize);
        // ...other things\ndomain_state->c_stack = NULL;
	```
    
    [Commit](https://github.com/ocaml/ocaml/blob/bdda743eb66456b659b9f70922a3cba867b57dd2/runtime/domain.c#L650-L651)
    
    `caml_alloc_main_stack` is a wrapper around `caml_alloc_stack_noexc`
    which calls `alloc_size_class_stack_noexc` - the indirections comes
    from stack caches and an internal policy to not raise OCaml exceptions
    in function names ending with \_noexc.
    
    `caml_call_realloc_stack`  allocations are not
    expensive and raw `malloc()` but cached ones - from a cache of recently
    freed stacks (often referred to as stack cache). You can read more
    about such strategies of stack allocation for continutations in "From
    folklore to fact: comparing implementations of stacks and
    continuations".

2.  Fibers

    Now that we know OCaml programs dont use a single program stack like
    they used to, but more than one, let's understand how they're
    managed. Crucial concept here is Fibers - not to be confused with the
    concurrency primitive. 
    
    Fibers are defined in the "Retrofitting effects.." paper as a tuple of
    frames and a handler. In `fiber.h`, there are organised with the
    struct, `stack_info`.
    
	```c
        struct stack_info {
          void* sp;            /* stack pointer of the OCaml stack when suspended */
          void* exception_ptr; /* exception pointer of OCaml stack when suspended */
        
          struct stack_handler* handler; /* effect handling state for the fiber */
        
          int cache_bucket;
          size_t size; /* only used when USE_MMAP_MAP_STACK is defined */
          uintnat magic;
          int64_t id;
        };
	```
    
    And stack<sub>handler</sub> contains the handlers for any effects or exceptions
    thrown for this stack - making `stack_info` the structure representing
    the state for a fiber.
    
	```c
        struct stack_handler {
          value handle_value;
          value handle_exn;
          value handle_effect;
          struct stack_info* parent; /* parent OCaml stack if any */
        };
	```

3.  When are Fibers allocated?

    They are allocated when,
    
    1.  Programmer has created a new delimited continutation with try<sub>with</sub>() or match<sub>with</sub>()
    2.  We have run out of space on the current stack
    
    The function `caml_alloc_stack` (visible to OCaml layer in effects.ml
    via alloc<sub>stack</sub>) calls `alloc_size_class_stack_noexc` which is an
    internal runtime function returning a new fiber (ie `stack_info`
    pointer).
    
    Notice, how `caml_alloc_stack` returns an OCaml value
	
    ```c
        value caml_alloc_stack (value hval, value hexn, value heff);
	```
    
    This is because it's the underlying c function of `alloc_stack` OCaml
    function in `effects.ml`.
    
    One can think of it as a OCaml layer friendly version of
    `alloc_size_class_stack_noexc`.

4.  How do Fibers relate to Stacks?

    Fibers, or the tuple `(frames, handlers)` are set up on the
    heap-allocated stacks. Here's a description of the fiber layout on a
    stack as documented in `fiber.h`
    
	```c
        /* Stack layout for native code. Stack grows downwards.
         *
         * +------------------------+
         * |  struct stack_handler  |
         * +------------------------+ <--- Stack_high
         * |    caml_runstack /     |
         * |   caml_start_program   |
         * +------------------------+
         * |                        |
         * .      OCaml frames      . <--- sp
         * |                        |
         * +------------------------+ <--- Stack_threshold
         * |                        |
         * .        Red Zone        .
         * |                        |
         * +------------------------+ <--- Stack_base
         * |   struct stack_info    |
         * +------------------------+ <--- Caml_state->current_stack
         */
    ```
    
    [Link to the source tree](https://github.com/ocaml/ocaml/blob/bdda743eb66456b659b9f70922a3cba867b57dd2/runtime/caml/fiber.h#L76-L94)

5.  How are frames created and added?

    How OCaml creates stack frames and allocates registers is a big topic in itself, but here are the bullet points.
    
    1.  OCaml has unified representation - a machine word can represent
        both integers and pointers. And OCaml makes it's best attempt to
        store them in the registers before calling the function (without
        necessarily creating a stack frame)
    2.  When there aren't enough registers, OCaml pushes these function
        arguments, that is the actual arguments, into the stack frames and
        makes the most of whatever registers are available. To get the full
        details of the available registers, we will have to look into the
        file `arm64/proc.ml`, which contains information about the
        available registers, calling conventions and other architecture
        specific stuff.
    3.  OCaml doesn't use frame pointers (rbp/x29) like in C (by
        default). Frame pointers are used very often to create stack
        traces, help debugger unwind frames during exceptions.
    
    For exceptions, OCaml maintains a linked list of stack pointers and program counters.
    For debuggers, OCaml generates DWARF expressions
    
    The compiler does have a `+fp` variant where frame pointers are pushed as a part of the stack frame, but this is opt-in.
    
    1.  Functions being called are free to clobber the registers for their
        needs. That is, no callee-saved registers. This is true for
        exception handling too, which we'll discuss shortly.

6.  Exceptions

    Functions are simple and easy to understand as a mechanism for control
    flow. C's goto's are simpler but makes it hard to reason about the
    codebase.
    
    Quick summary of functions: they view the code as a tree and try to do a
    depth-first traversal. They use stacks to create frames that manage
    the state of a given function as it executes, and allocate all the
    static, i.e. ahead-of-time, known memory requirements. They grow as we
    go deeper into the function call-tree and shrink in size as the
    control flow goes back to the root of the program.
    
    Exceptions introduce the first level of non-local control flow, and in order to do so, "cut" stacks when exceptions are raised. Let's take a look at how this happens:
    
    Compiling exceptions has two parts to it:
    
    1.  Installing the handlers when a try/catch block is encountered.
    2.  Unwinding the stack when a raise/throw is encountered.
    
    During the installation phase, the compiler records the stack state
    which we can fall back to when the program throws the
    exception. HereÕs an example of the assembly generated.
    
    For reference, in `meander.ml` we has,
    
	```ocaml
        let omain v =
          let { a; b } = ocaml_fn v (v + 1) in
          Printf.printf "%d" (a + b);
        try (* h1 *)
          (try  (ocaml_to_c ()) (* h2 *)
              with E2 -> 0)
         with E1 -> print_endline "In E1"; v
	 ```
    
    And the generated assembly around the try/catch looks like the following:
    
	```asm
        adr	x16, L108
        str	x26, [sp, -16]!
        str	x16, [sp, #8]
        mov	x26, sp
        adr	x16, L111
        str	x26, [sp, -16]!
        str	x16, [sp, #8]
        .cfi_adjust_cfa_offset	16
        mov	x26, sp
        orr	x0, xzr, #1
        .loc	1	17
        adrp	x8, _ocaml_to_c@GOTPAGE
        ldr	x8, [x8, _ocaml_to_c@GOTPAGEOFF]
        bl	_caml_c_call
    ```
    
    In arm64, `x26` is the trap pointer (you can find the complete
    reference in `arm64/proc.ml`). Trap pointer here refers to the address
    where exception handlers reside - i.e. the code that analyzes the
    throw exception and decides what action to take.
    
    For context, `L108` is where the exception handler for outer `try` resides.
    
    The compiler.
    
    1.  Pushes the last trap pointer to the stack `(sp - 16)!`
    2.  Pushes current try blocks corresponding exception handler to stack
    3.  Saves a copy of current stack pointer as current trap pointer
    
    All this is for the outer try block. Then, for the inner try block, it again,
    
    1.  Saves inner exception handler, at `L111`, to x16 (temp register)
    2.  Pushes  current trap pointer to stack
    3.  Pushes L111Õs address (loaded in x16) onto the stack.
    4.  Make the current stack pointer, the current trap pointer.
    
    We note that, the runtime,
    
    1.  Only concerns itself with where to find the last entered try block's exception handler
    2.  What was the stack pointer state as we entered the last try block
    3.  It has a linked list of exception handlers on the stack. And it knows
    
    where to find the stack pointer to cut to right next to this linked
    list's nodes. 
    
    All this information is used by the `raise_exception` function defined
    in the assembly part of the runtime. Why assembly? Because if written
    in C, it would create it's own stack and not let us control it. We
    need to craft stack frames to our liking!
    
	```
                CFI_STARTPROC
            /* Test if backtrace is active */
                ldr     TMP, Caml_state(backtrace_active)
                cbnz    TMP, 2f
        1:
                JUMP_TO_TRAP_PTR
        2:  /* Zero backtrace_pos */
                str     xzr, Caml_state(backtrace_pos)
        L(caml_reraise_exn_stash):
            /* Preserve exception bucket in callee-save register x19 */
                mov     x19, x0
            /* Stash the backtrace */
                                       /* arg1: exn bucket, already in x0 */
                mov     x1, x30        /* arg2: pc of raise */
                mov     x2, sp         /* arg3: sp of raise */
                mov     x3, TRAP_PTR   /* arg4: sp of handler */
            /* Switch to C stack */
                ldr     TMP, Caml_state(c_stack)
                mov     sp, TMP
                bl      G(caml_stash_backtrace)
            /* Restore exception bucket and raise */
                mov     x0, x19
                b       1b
                CFI_ENDPROC
                END_FUNCTION(caml_raise_exn)
	```
    
    1.  Checks if it needs to collect backtrace. We'll skip this and
        instead focus on the stack management.
    2.  `JUMP_TO_TRAP_PTR` is a macro - it cuts the stack to the point
        where the nearest exception handler is.
    3.  `caml_stash_backtrace` is a C function defined in `backtrace_nat.c`
        Lines after `JUMP_TO_TRAP_PTR` prepare the registers and the stack
        for this C function, in accordance with C's calling conventions.
    
    Let's now look at `JUMP_TO_TRAP_PTR`
	
	```
    
        .macro JUMP_TO_TRAP_PTR
            /* Cut stack at current trap handler */
                mov     sp, TRAP_PTR
            /* Pop previous handler and jump to it */
                ldr     TMP, [sp, 8]
                ldr     TRAP_PTR, [sp], 16
                br      TMP
        .endm
	```
    
    1.  Simply set the stack pointer to state stack pointer would be when
        executing the exception handler.
    2.  Jump to the exception handler which is present on the stack -
        `(sp + 8)`
    3.  Before making the jump, set the next exception handler to `sp + 16`
        because it contains the parent exception handler of this enclosing
        try/catch block.
    
    What happens at the exception handler? Back to `meander.s`!
    Recall, the try/catch blocks looked like this,
    
	```
        try (* h1 *)
          (try  (ocaml_to_c ()) (* h2 *)
              with E2 -> 0)
         with E1 -> print_endline "In E1"; v
    
    Inner most exception handler looks like this,
    
        L111:
                adrp	x15, _camlMeander@GOTPAGE
                ldr	x15, [x15, _camlMeander@GOTPAGEOFF]
                ldr	x19, [x15, #8]
                cmp	x0, x19
                b.ne	L110
                orr	x0, xzr, #1
                b	L109
	```
    
    `x0`, at this point, contains the result of `ocaml_to_c` call. We
    know, as we wrote `ocaml_to_c`, that it raises an exception. So unlike
    regular functions, it isn't going to return and shrink the stack. Will
    call `caml_raise_exn` and cut the stack to it's handler. This is how
    control gets passed this block of asm (at L111). `x0` contains the
    raised value, `E1`. The handler compares the received value against
    the exception declaration present in the global data section with the
    help of a offset table. If true, handler executes the handling
    expression (`E2 -> 0`)
    
    We notice, just like functions, exceptions are light-weight. Languages
    like C/C++ often need to respect calling-conventions and have
    callee-saved  registers to do this. In C/C++ caller expects certain
    registers to be un-clobbered whereas OCaml doesn't enforce this, which
    makes installing and running an exception handlers lighter, least
    compared to C/C++, by generating less prologue/epilogue code the
    save/restore states.
    
    Notice also how the compiler pushes the program counter and the current
    stack pointer into the stack every time it encounters a try/block:
    very similar to what it would do when it encounters a function in a
    manner of speaking. Try catch are very similar but very similar to functions with a key difference
    being the linearity or how the linearity of the flow: functions always
    return and exceptions don't. When we see a function `a` calling b,
    calling `c`, we can only return to `a` after `b` and `c` have finished and this
    is what we mean by linearity this is what we are referring to by
    linearity over here. On the other hand as we know exceptions jump from
    one place to another. Therefore exceptions have to create frames just
    like functions too but also discard frames which a function never has
    to do because function frames are going to grow and shrink in the last
    in first out order. Exceptions therefore have to associate the nearest
    try catch block with the exception being raised and this happens by
    every raise function knowing where the catch handler is installed in
    the memory and OCaml has a special pointer called the trap pointer to
    keep track or keep track of this exception nearest exception
    handler.
    
    What happens when raise exception doesn't match with any handler? Re-raise!


## TODO insert code snippet


# TODO How are effects handled? Records on stack

# TODO What does an OCaml frame look like?


