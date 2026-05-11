- **const**: Makes a variable's value unchangeable after initialization.

- **typedef**: Creates an alias for an existing data type to make code more readable and portable.

---

- **stack frame**: A memory block created for a function call. It stores local variables, parameters, and the return address. It is removed when the function ends.
- **call stack**: A data structure that tracks function calls by storing each call's stack frame.

- **scope**: Where a variable can be accessed, such as inside a function, within a file, or across files. (accessibility)
- **lifetime**: How long a variable exists in memory. Local variables live while their block is active; file-scope variables live for the whole program. (existence)

- **file scope**: The scope of an identifier declared outside any function. It is valid from its declaration to the end of the source file.
- **local scope (parameter / block variable)**: The scope of an identifier declared inside a block. It is valid from its declaration to the end of that block.
- **global scope**: An informal term for identifiers accessible across multiple source files, often via `extern`.
- **nested scope**: A scope inside another scope. Outer-scope identifiers are accessible in the inner scope.
- **static**: Keeps a local variable's value between function calls, or limits a file-scope identifier to the current file.
- **extern**: Tells the compiler that a variable or function is defined in another file.

---

- **process**: A running program managed by the operating system.
- **address space**: The memory range a process can use, including the stack, heap, data segment, and code segment.
  - **stack**: Stores local variables and function parameters. They are allocated when a function is called and deallocated when it returns.
  - **heap**: Stores dynamically allocated memory. It is allocated with `malloc()` or `calloc()` and freed manually with `free()`.
  - **data segment**: Stores global and static variables. It is usually divided into initialized and uninitialized parts.
    - **static variables**: Declared with `static`. They keep their value between function calls or are limited to the current file.
    - **global variables**: Declared outside any function. They can be accessed throughout the program and from other files with `extern`.
  - **code segment**: Stores the program's instructions. It is often read-only.
  - **free space**: Space between the stack and heap. It shrinks as they grow.
  - **memory leak**: Dynamically allocated memory that is never freed.
  - **stack overflow**: The stack runs out of space, often due to deep or infinite recursion.

---

- **pointer**: A variable that stores a memory address.
- **wild pointer**: A pointer that is uninitialized or points to invalid memory.
- **struct**: A user-defined type that groups variables of different types into one unit.

---

- **function declaration**: Tells the compiler a function's name, return type, and parameters, without the body.
- **function definition**: Provides the function body and implementation.
- **function parameters**: Variables in a function definition that receive values when the function is called.
- **function arguments**: The actual values passed to a function call.
- **literal value**: A fixed value written directly in code, such as a number or string.
- **void**: Shows that a function returns no value.

- **local / parameter variable allocation**: When a function is called, its local variables and parameters are placed in a stack frame. When the function ends, that stack frame is removed automatically.

- **linking**: Combines object files and libraries into one executable and resolves addresses.
- **abstraction**: Hides implementation details and exposes only what is needed.
- **imperative programming**: Uses statements that change program state and describe how to do a task.
- **object file**: A compiled file (`.o` on Unix, `.obj` on Windows) containing relocatable machine code.
- **virtual address space**: A process's private memory range, mapped to physical memory by the operating system.
- **escape character**: A special character that changes how the following character is interpreted. Usually `\`.
- **overflow**: Happens when a variable is given a value it cannot hold.

- **build process**:
  - Write C code in a `.c` file.
  - **preprocessing**: Handles directives like `#include` and `#define`.
  - **compilation**: Translates preprocessed code into assembly.
  - **assembly**: Converts assembly into object code.
  - **linking**: Combines object code and libraries into an executable.
  - **execution**: The operating system loads and runs the program.

- **arithmetic promotion**: In mixed-type arithmetic, the lower type is usually promoted to the higher type.

```c
char ch = (num % 10) + '0'; // int -> char
int num = '5' - '0';        // char -> int
```

### Strings

- `strlen(char *str)`: Returns the length of a string, excluding `'\0'`.
- `strcpy(char *dest, char *src)`: Copies `src` to `dest`, including `'\0'`. `dest` must have enough space.

```c
char *copyStr(char *src) {
    char *dest = malloc((strlen(src) + 1) * sizeof(char));
    strcpy(dest, src);
    return dest;
}

char *str1 = "hello";
char *str2 = "world";
char *result = malloc(strlen(str1) + strlen(str2) + 1); // +1 for '\0'
strcpy(result, str1);
strcat(result, str2);
```

- `strcmp(char *str1, char *str2)`: Compares two strings lexicographically. Returns `0` if equal, negative if `str1 < str2`, and positive if `str1 > str2`.

```c
char *str1 = "hello";
char *str2 = "world";
int result = strcmp(str1, str2); // negative because "hello" < "world"
```

### Struct

- `malloc(n * sizeof(type))`
- `calloc(n, sizeof(type))`
- `realloc(oldptr, n * sizeof(type))`

Example from 2022 POP:

```c
typedef struct file {
    char *name;
    int size;
} File;

typedef struct directory {
    char *name;
    File **files; // array of pointers to File
    int fileCount;
} Directory;

File *createFile(const char *name, const int size) {
    File *file = calloc(1, sizeof(File));

    file->name = calloc(strlen(name) + 1, sizeof(char));
    strcpy(file->name, name);

    file->size = size;
    return file;
}

Directory *createDirectory(const char *name) {
    Directory *dir = calloc(1, sizeof(Directory));

    dir->name = calloc(strlen(name) + 1, sizeof(char));
    strcpy(dir->name, name);

    dir->files = NULL; // empty array of pointers to File
    dir->fileCount = 0;

    return dir;
}

void addFile(Directory *dir, File *file) {
    dir->fileCount++;
    dir->files = realloc(dir->files, dir->fileCount * sizeof(File *));
    dir->files[dir->fileCount - 1] = file;
}

void addFile_(Directory *dir, File *file) {
    dir->fileCount++;

    File **newFiles = calloc(dir->fileCount, sizeof(File *));

    for (int i = 0; i < dir->fileCount - 1; i++) {
        *(newFiles + i) = *(dir->files + i); // copy old files to new array
    }

    *(newFiles + (dir->fileCount - 1)) = file; // add new file
    free(dir->files);
    dir->files = newFiles;
}
```

---

```c
typedef struct Node {
    int data;
    char *str;
    double *doubleArr;
    int intArr[10];
} Node;

Node *createNode(
    const int data,
    const char *str,
    const double *doubleArr,
    const int doubleSize,
    const int *intArr
) {
    Node *node = malloc(sizeof(Node));

    node->data = data;

    node->str = malloc(strlen(str) + 1);
    strcpy(node->str, str);

    node->doubleArr = malloc(doubleSize * sizeof(double));
    for (int i = 0; i < doubleSize; i++) {
        node->doubleArr[i] = doubleArr[i];
    }

    for (int i = 0; i < 10; i++) {
        node->intArr[i] = intArr[i];
    }

    return node;
}
```
