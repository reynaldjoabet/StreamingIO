# StreamingIO
Instead of using concurrency to allow multiple users to use the same computer at the same time,it was used to allow a single user  running multiple programs at the same time

Inside the CPU, there are special registers like the instruction register and the address register

The address register holds the memory location of the next instruction the CPU will execute

When the CPU is ready for the next instruction,it fetches this value and copies it to the instruction register
The CPU then decodes this to know what to do next eg addition, copy operation, whatever

After the instruction is executed, the address register value increases, pointing to the next instruction

Fetch decode execute

A program starts, the main function serves as the entry point, each time a new variable is encountered, its value is stacked into the designated region(memory)

the memory address marking the beginning of the stack is known as the `stack origin` while the `stack pointer` indicates the current topmost datum on the stack.
The stack pointer is stored somewhere

To write something onto the stack, all that is required is to fetch the stack pointer address, add 1 to that value and the result is the memory address where we can write more data

when a function is called, all its local values are pushed unto the stack. it is crucial to remember the starting point of these local values because when the function concludes, we need to reset the stack pointer to its position just before the function call

when a function calls itself,new stack frames are successively pushed into the stack for each recursive call

 The stack pointer holds the address of the top element of the stack (the most recently pushed item). It moves up or down depending on whether items are being added (pushed) or removed (popped) from the stack

 The stack origin might be at memory address `0x7FFFFFFF`, with a stack limit at `0x7FFF0000`, allowing for a `1MB stack`.
The stack pointer starts near the origin (e.g., `0x7FFFFFFF`) and moves downward as the stack grows.
When the stack pointer reaches the limit (`0x7FFF0000`), attempting to push more data will result in a stack overflow.

[WHY IS THE STACK SO FAST?
](https://www.youtube.com/watch?v=N3o5yHYLviQ)

You should depend on cats explicitly anyways since you are using it directly

-Xms1G
-Xmx6G
-Xss8M

 FlatMap can now be tail recursive and avoid stackoverflow errors,

  inline can appear as 
  - inline def
  - inline parameter
  - transparent inline def
  - inline if
  - inline match

  the new syntax for typeclass combines traits ( to define the abstraction),regular and extension methods and given instances( for type class instances)