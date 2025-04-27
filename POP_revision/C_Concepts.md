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

**linking**
**abstraction**
**Escape character** Special character used to force an alternate interpretation of the following
character. It is typically "\" although others exist
**Overflow**: Occurs when a variable is assigned a value that it cannot hold

In C, when an operation involves different arithmetic types, the "lower" type is typically promoted to the "higher" type.

```c
 char *ptr = (num % 10) + '0'; // int -> char
 int num = '5' - '0'; // char -> int
```

### String

`strlen(char *str)`: Returns the length of a string excluding the '\0'.
`strcpy(char *dest, char *src)`: Copies the string from `src` to `dest`, including the '\0' character. The destination must have enough space to hold the source string.
```c
char* coryStr(char* src){
    char *dest = malloc((strlen(src) + 1) * sizeof(char)); 
    strcpy(dest, src);

    return dest;
}

char *str1 = "hello";
char *str2 = "world";
char *result = malloc(strlen(str1) + strlen(str2) + 1); // +1 for null terminator
strcpy(result, str1); // Copy str1 to result
strcat(result, str2); // Concatenate str2 to result
```

---
`strcmp(char *str1, char *str2)`: Compares two strings lexicographically.   
Returns 0 if equal, negative if str1 < str2, and positive if str1 > str2.
```c
char *str1 = "hello";
char *str2 = "world";
int result = strcmp(str1, str2); // result will be negative since "hello" < "world"
```

### struct
`malloc(n * sizeof(type))`
`calloc(n, sizeof(type)`
`realloc(oldptr, n * sizeof(type))`
Example of 2022 POP: 
```c
typedef struct file{
    char *name;
    int size;
}File;

typedef struct directory{
    char *name;
    File **files; // array of pointers to File
    int fileCount;
}Directory;



File *createFile(char *name, int size){
    File *file = calloc(1, sizeof(File));

    // NOTE HERE //
    file->name = calloc(strlen(name) + 1, sizeof(char));
    strcpy(file->name, name);
    // NOTE HERE //

    file->size = size;

    return file;
}

Directory *createDirectory(char *name){
    Directory *dir = calloc(1, sizeof(Directory));

    dir->name = calloc(strlen(name) + 1, sizeof(char));
    strcpy(dir->name, name);

    dir->files = NULL; // enpty array of pointers to File
    dir->fileCount = 0;

    return dir;
}

void addFile(Directory *dir, File *file){

    dir->fileCount++;

    dir->files = realloc(dir->files, (dir->fileCount) * sizeof(File*));
    dir->files[dir->fileCount - 1] = file; 
}

void addFile_(Directory *dir, File *file){
    dir->fileCount++;

    File **newFiles = calloc(dir->fileCount, sizeof(File*));

    for(int i = 0; i < dir->fileCount - 1; i++)
        *(newFiles + i) = *(dir->files + i); // copy old files to new array
    
    *(newFiles + (dir->fileCount - 1)) = file; // add new file
    free(dir->files);
    dir->files = newFiles; // update the pointer to the new array
}
```



