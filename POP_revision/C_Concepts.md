- **const**: It makes a variable' value unchangeable after it's initialization, preventing accidental changes.
- **static**: It makes a variable or function keep its value between function calls, or restricts its visibility to the file it's defined in.
- **extern**: It tells the compiler that a variable or function is defined in another file, so it can be used in the current file.
- **typedef**: It creates an alias for a existing data type to make code more readable, portable.
---

- **stack frame**: It's a memory block created for a function call to its local variables, parameters and return addresses, etc. When the functions ends, the stack frame is removed.
- **call stack**: It's a data structure that keeps track of function calls in a program, storing information about each call's stack frame.

- **scope**: It defines where a variable can be accessed in the code, like inside a function (local variable), file scope(within a file) or global scope (entire program and other files).(NOTE: about accessibility)
- **lifetime**: The duration a variable exists in memory, from its creation to destruction. Local variables' lifetime lasts while their block active; file scope variables last the entire program runtime.(NOTE: about existence)
  
---
- **address space**: It's the range of memory a program can use, divided into areas like stack, heap, data segment and code segment.
  - **stack**: The stack stores local variables and function parameters. These ara allocated when a function is called and deallocated when the function returns.
  - **heap** : The heap stores dynamically allocated memory, which is allocated using `malloc() or calloc()` and deallocated using `free()` manually.
  - **data segment**:divided into initialized and uninitialized parts.
    - **static variables**: Declared with the `static` keyword, they keep its value between function calls or restricts it visibility to the file it's defined in, with file scope.
    - **global variables**: Declared outside of any function, they can be accessed from anywhere in the program and other files.
  - **code segment**: Stores the program's instructions, which can be read-only or in ROM/flash.
  - **free space**: Between the stack and heap. Decreased as stack and heap grow.
  - **memory leak**: Occurs when dynamically allocated memory is not freed, causing the program to use more memory over time.
  - **stack overflow**: stack runs out of memory due to too many function calls without returning, often from infinite recursion.
  - **compile**: Converts source code into machine code, creating an executable file

---
- **pointer**: A variable that store a memory address, used to access or modify data directly.
-  **wild pointer**:A pointer that has not been initialized or points to an invalid memory, leading to undefined behavior.
 - **struct**: A user-defined data type that groups variables of  different types into a single unit.
---
- **function declaration**: It tells the compiler about function's name, return type and parameters without providing the implementation.
- **function definition**: It provides the actual implementation of a function, including its body and logic.
- **function parameters**: variables(or say placeholders) in function definition that used to receives values when the function is called.
- **function arguments**: the actual values passed to a  function when it is called.
- **literal value**: a fixed value written directly in the code, like a number or a string.
- **void**: It tells the compiler that a function does not return any value.


**How local and parameter variables are allocated and deallocated in memory when a function is called**: When a function is called, its local variables and parameters are allocated on the stack in a stack frame. When the function ends, the stack frame is removed, deallocating the memory automatically.