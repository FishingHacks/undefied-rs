# Specs

# Intrinsics

| Name          | Symbol     | Stack Layout                | Stack Pushes |                                                                                                                                                       Description |
| ------------- | ---------- | --------------------------- | ------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------: |
|               |            |                             |              |                                                                                                                                                **Util Functions** |
| Print         | print      | int                         | _none_       |                                                                                                                  Print the top number of the stack (+ a new line) |
| FileLocation  | here       | _none_                      | Str          | Puts the string representation of the current file, line and character (at the start of the 'here' token) as a built-in String Representation, ready for printing |
| StackInfo     | ???        | _none_                      | _none_       |                                                                          Halts the program and prints the current Stack (Types-only). Happens during Typechecking |
|               |            |                             |              |                                                                                                                                                    **Operations** |
| Add           | +          | int int                     | int          |                                                                                                                                     Add the 2 integers at the top |
| Subtract      | -          | int int                     | int          |                                                                                                                                Subtract the 2 integers at the top |
| Multiply      | \*         | int int                     | int          |                                                                                                                                Multiply the 2 integers at the top |
| Div-Mod       | /%         | int int                     | int int      |                                                                                                                          Divide and Mod the 2 integers at the top |
|               |            |                             |              |                                                                                                                                              **Stack Operations** |
| Drop          | drop       | any                         | _none_       |                                                                                                                                             discard the top value |
| Duplicate     | dup        | T                           | T T          |                                                                                                                                           Duplicate the top value |
| Over          | over       | T1 T2                       | T1 T2 T2     |                                                                                                                                 Copy the 2nd top value to the top |
| Swap          | swap       | T1 T2                       | T2 T1        |                                                                                                                                    Swap the top and 2nd top value |
| Rotate        | rot        | T1 T2 T3                    | T3 T1 T2     |                                                                                                                       Move the top value behind the 3rd top value |
|               |            |                             |              |                                                                                                                                                   **Comparisons** |
| LessThan      | <          | int int                     | bool         |                                                                                                             Check if the top value is less than the 2nd top value |
| LessThanEq    | <=         | int int                     | bool         |                                                                                                 Check if the top value is less than or equal to the 2nd top value |
| GreaterThan   | >          | int int                     | bool         |                                                                                                          Check if the top value is greater than the 2nd top value |
| GreaterThanEq | >=         | int int                     | bool         |                                                                                              Check if the top value is less greater or equal to the 2nd top value |
| Equal         | =          | int int                     | bool         |                                                                                                              Check if the top value is equal to the 2nd top value |
| NotEqual      | !=         | int int                     | bool         |                                                                                                          Check if the top value is not equal to the 2nd top value |
|               |            |                             |              |                                                                                                                                                  **Instructions** |
| Syscall1      | syscall1   | int any                     | int          |                                                                                                              Invoke the syscall-assembly instruction with 1 value |
| Syscall2      | syscall2   | int any any                 | int          |                                                                                                             Invoke the syscall-assembly instruction with 2 values |
| Syscall3      | syscall3   | int any any any             | int          |                                                                                                             Invoke the syscall-assembly instruction with 3 values |
| Syscall4      | syscall4   | int any any any any         | int          |                                                                                                             Invoke the syscall-assembly instruction with 4 values |
| Syscall5      | syscall5   | int any any any any any     | int          |                                                                                                             Invoke the syscall-assembly instruction with 5 values |
| Syscall6      | syscall6   | int any any any any any any | int          |                                                                                                             Invoke the syscall-assembly instruction with 6 values |
| Load          | @8         | ptr                         | int          |                                                                                                                                                Load a 8-bit value |
| Store         | !8         | ptr int                     | _none_       |                                                                                                                                               Store a 8-bit value |
| Load16        | @16        | ptr                         | int          |                                                                                                                                               Load a 16-bit value |
| Store16       | !16        | ptr int                     | _none_       |                                                                                                                                              Store a 16-bit value |
| Load32        | @32        | ptr                         | int          |                                                                                                                                               Load a 32-bit value |
| Store32       | !32        | ptr int                     | _none_       |                                                                                                                                              Store a 32-bit value |
| Load64        | @64        | ptr                         | int          |                                                                                                                                               Load a 64-bit value |
| Store64       | !64        | ptr int                     | _none_       |                                                                                                                                              Store a 64-bit value |
| Arg0PtrPush   | argv       | _none_                      | ptr          |                                                                                                                              Push the pointer of the 0th argument |
|               |            |                             |              |                                                                                                                                                        **Typing** |
| CastPtr       | cast(ptr)  | any                         | ptr          |                                                                                                                             Cast the top value to a pointer (ptr) |
| CastInt       | cast(int)  | any                         | int          |                                                                                                                             Cast the top value to a integer (int) |
| CastBool      | cast(bool) | any                         | bool         |                                                                                                                            Cast the top value to a boolean (bool) |
|               |            |                             |              |                                                                                                                                            **Bitwise Operations** |
| ShiftLeft     | shl        | int int                     | int          |                                                                                                                                    Perform a left shift operation |
| ShiftRight    | shr        | int int                     | int          |                                                                                                                                   Perform a right shift operation |
| And           | and        | int int                     | int          |                                                                                                                                                    And 2 integers |
| Or            | or         | int int                     | int          |                                                                                                                                                     Or 2 integers |
| Xor           | xor        | int int                     | int          |                                                                                                                                                    Xor 2 integers |
| Not           | not        | int                         | int          |                                                                                                                                        Perform a not on a integer |

# Keyword Specifications

## Include

```rb
include "string-or-module"
include "string-or-module"c
```

Example:

```rb
include "@std/io"
include "./fileparser"c
```

## If

stack pops: `bool`

code: Code

```rb
if <code> end
if <code> else <code> end
if <code> else <code> if* <code> else <code> end
```

Example:

```rb
include "@std/io"

true if 1 print end
1
     dup 3 = if  drop "three" puts
else dup 2 = if* drop "two" puts
else dup 1 = if* drop "one" puts
else             drop "idk" puts
end

# lnot lol
false if false else true end
```

## While

check should not change the stack, except for adding a bool (which determines if the loop is being ran)

code should not change the stack

```rb
while <check> do <code> end
```

```rb
10
while dup 0 > do
  dup print
  1 -
end drop
```

counts down from 10-1:

```
10
9
8
7
6
5
4
3
2
1
```

## Memory Creations

can be top-level or in a function for local memory; Please keep local memory small and use an allocator for larger structures or arrays

name: word
simplified-code: ConstMemoryCode

```rb
memory <name> <simplified-code> end
```

```rb
include "@std/str"

memory NameAndVersionString sizeof(Str) 2 * end
```

## Constant Definition

name: word
simplified-code: ConstMemoryCode

```rb
const <name> <simplified-code> end
```

> **Offsetting**
>
> Constants support offsettings. This means, that when you specify "offset" or "reset" in the `<simplified-code>`, it will take the current offset and add the original value to it.
> This is useful for enums.

**Example**

```rb
const undefiedStdLib->version 1 end
```

**Offsetting Example**

```rb
const Keyword.If 1 offset end
const Keyword.IfStar 1 offset end
const Keyword.Else 1 offset end
const Keyword.Struct 1 offset end
const Keyword.End 1 offset end
const Keyword.While 1 offset end
const Keyword.Const 1 offset end
const Keyword.Memory 1 offset end
const Keyword.Include 1 offset end
const Keyword.Fn 1 offset end
const Keyword.In 1 offset end
const Keyword.Splitter 1 offset end
const Keyword.Do 1 offset end
const Keyword.Call 1 offset end
const Keyword.Sizeof 1 offset end
const Keyword.AddrOf 1 offset end
const Keyword.CallAlike 1 offset end
const Keyword.length 1 reset end
```

## Function definitions

name: Word
intypes: Type|StructName
outtypes: Type|StructName
code: Code

```rb
fn <name> in <code> end
fn <name> <intypes> in <code> end
fn <name> <intypes> -- <outtypes> in <code> end
fn <name> <intypes> -- in <code> end
fn <name> -- <outtypes> in <code> end
```

### Inline

When adding a `inline`-keyword before the `function`-keyword, then the function will get inlined. That means instead of adding a call instruction, it will paste the function body to every usage of the function. Only recommended for small functions, such as puts or eputs.

**Example**

```rb
inline fn 1plus int -- int in 1 + end

fn main in 10 1plus print end
```

This will result in:

```rb
fn main in 10 1 + print end
```

## Return

Stack Requirements: &lt;function return-types>

```rb
return
```

**Example**

```rb
include "@std/str"

fn toStr
  int
  --
  Str
in
       dup 0 = if  drop "Zero"    return
  else dup 1 = if* drop "One"     return
  else dup 2 = if* drop "One"     return
  else dup 3 = if* drop "One"     return
  else dup 4 = if* drop "One"     return
  else dup 5 = if* drop "One"     return
  else             drop "unknown" return
  end
end
```

## typefence

A typefence; useful for asserting a stack layout

```rb
typefence <types> end
```

**Example**

```rb
typefence int int int end # ensures that the stack has 3 ints
```

## Assembly

Embed custom assembly

> note: in-types: these types is what the assembly expects on the stack and will use
>
> note: out-types: these types is what the assembly will leave behind on the stack after the operations

```rb
assembly <in-types> -- <out-types> in "<assembly>"... end
assembly <in-types> in "<assembly>"... end
assembly -- <out-types> in "<assembly>"... end
assembly in "<assembly>"... end
```

**Example**

```rb
# custom bitwise and implementation that's the same as the default one but more bloat cuz function
fn bitwiseAnd int int -- int in
  assembly int int -- int in
    "pop rax"
    "pop rbx"
    "and rbx, rax"
    "push rbx"
  end
end
```

## Undef

Undefine macros (inline functions), functions and constants. Note: this won't delete the identifier. Older usages won't get replaced.

identifier

```rb
undef <identifier>
```

**Example**

```rb
const a 10 end
undef a
const a 11 end

inline fn nums 101 end
undef nums
inline fn nums 110 111

fn main in
  a print # 1
  nums print print # 111 110
end
```

```rb
const a 10 end
const oldA a end
inline fn oldAMacro a end
fn oldAFunction a end
undef a

const a 11 end
const newA a end
inline fn newAMacro a end
fn newAFunction a end

fn main in
  oldA           print # 10
  oldAMacro      print # 10
  oldAFunction   print # 10
  newA           print # 11
  newAMacro      print # 11
  newAFunction   print # 11
end
```

## .log, .info, .warn, .error

Preprocessor: Log a value to the console.

```rb
.log|.info|.warn|.error <constant>|string|number|cstring
```

**Example**

```rb
.ifdef tmp_mem .error "tmp_mem is already defined :c" .end

memory tmp_mem 10 1024 * end # allocate 10kb
```

## .ifdef, .ifndef

Preprocessor: Checks if a memory, function, macro (inline function) or constant is defined or not defined

note: due to the nature of being able to redefine local memories, they are **not** included

code: Code

```rb
.isdef|.isndef <identifier> <code> .end
```

**Example**

```rb
.isdef __LNX__ .log "Using Linux" .end
```

## .is, .isn

Preprocessor: Checks if a identifier is a specific thing (or not)

If the word was not defined, this will yield false in .is and true in .isn

type: macro, fn, const, memory, local_memory, intrinsic or keyword (multiple can be supplied, split using `|`)

```rb
.is|.isn <identifier> <type>
```

**Example**

```rb
.is macro|fn puts .log "puts is runnable" .end
.isn macro|fn puts .log "puts is not runnable" .end
```

## .ifc, .nifc

Preprocessor: Evaluates a very simple expression. Note: all values are treated as integers. true: 1, false: 0

`.if`: skips the block if the evaluated expression is false

`.nif`: skips the block if the evaluated expression is true

> **note**: for strings, only = and != is allowed
>
> **note**: for macros, they're only allowed to evaluate to 1 expression: either a string, constant or number

```rb
.ifc|.nifc <const|number|string|macro> <const|number|string|macro> <comparison>
```

comparison: One of the following: <, >, <=, >=, =, !=

## .if, ifn

Preprocessor: Checks if a constant is true/1

```rb
.if|.ifn <constant>
```

**Example**

```rb
.if __DEV__ .error "Compilation in devmode is disallowed" .end
.ifn __DEV__ .info "Compilation outside devmode is allowed" .end
```

## .str, .cstr

returns a string literal of a constant or value

```rb
.str|.cstr <const|number>
```

**Example**

```rb
const a 1 end

fn main in
  # regular strings
  .str a "1" str::eq not if "" panic end
  .str true "true" str::eq not if "" panic end
  .str 1 "1" str::eq not if "" panic end

  # c-style strings
  .cstr a "1"c cstr::eq not if "" panic end
  .cstr true "true"c cstr::eq not if "" panic end
  .cstr 1 "1"c cstr::eq not if "" panic end
end
```

## .pragma

Mark the inclusion behavior of this file. Default: once

```rb
.pragma once|multiple
```

**Example**

/once.undefied

```rb
# This is the default behavior, this doesn't need to be specified
.pragma once

.log "once"
```

/multiple.undefied

```rb
.pragma multiple

.log "multiple"
```

/main.undefied

```rb
include "./once"
include "./once"
include "./once"
include "./multiple"
include "./multiple"
include "./multiple"
```

The output during compilation should look something like this:

```
Info: /once.undefied:4:1: once
Info: /multiple.undefied:3:1: multiple
Info: /multiple.undefied:3:1: multiple
Info: /multiple.undefied:3:1: multiple
[INFO] [CMD]: Running nasm -felf64 -o "/main.o" "/main.asm"
[INFO] [CMD]: Running ld -o "/main" "/main.o"
[INFO] [CMD]: Running "/main"
```

## .has_param, .hasn_param, .check_param, .checkn_param

Checks if a function has the specified parameter (and value)

```rb
.has_param <function_name> <parameter>
.hasn_param <function_name> <parameter>
.check_param <function_name> <parameter> <value>
.checkn_param <function_name> <parameter> <value>
```

**Example**

/std/lnx.undefied:

```rb
.param __fn_type__
fn fputs int ptr int -- int in
  SC_write syscall3
end

.param __system__ "linux"
fn current_target_name end
```

/std.undefied:

```rb
.ifeq __TARGET__ __TARGET_LINUX__
    include "./std/lnx"
    .checkn_param current_target_name __system__ "linux" .error "imported linux but didnt find linux?" .end
    .has_param fputs __export__ .error "fputs is not allowed to be externally imported" .end
.end
```

## .get_param

Get's the value of a parameter; errors if the parameter doesn't have a value (defined using .param instead of .defparam) or if the parameter isnt defined

```rb
.get_param <function_name> <parameter>
```

**Example**

/std/lnx.undefied:

```rb
.param __system__ "linux"
fn fputs int ptr int -- int in
  SC_write syscall3
end

.param __system__ "linux"
fn __std_meta in end
```

/std/lnx/std_name.undefied:

```rb
include "@std/io"
include "@std/str"
.ifeq __TARGET__ __TARGET_LINUX__ include "./lnx" .end

.param __run_function__
.param __fn_anon__
fn a in
    memory printf_list sizeof(cstr) 1 list::size end

    # put the value of the "__system__" parameter of __std_meta (name of the current os, __std_meta is a function holding metadata for the std lib) in the printf_list list
    # steps: grab the variable, convert it to a cstring and then put it into the printf_list
    # printf accepts: ptr (to the list) ptr (c-style string)
    .get_param __std_meta __system__ Cstr::from_str printf_list list::insert_into
    "Compiled with %s!\n"c
end
```

## .end

Ends a preprocessor block

## .param, .defparam

Set a parameter for a block

Supported Blocks:

-   `assembly`
-   `fn`

**NOTE**: Only fn, not inline fn

```rb
.param <identifier>
.defparam <identifier> <value>
```

**Example**

```rb
include "@std/target"

.param deprecated
fn deprecated in end

.ifeq __TARGET__ __TARGET_LINUX__
fn main -- int in
  .param __supports_linux__
  assembly -- int in "push 10" end # Puts int 10 to exit with non-zero exitcode 10
end
.endif
.ifeq __TARGET__ __TARGET_JAVASCRIPT__
include "@std/io"

.param __eventlistener__
.defparam __event__ "resize"
.param __export__
fn onResize int int in
  "Resized\n" puts
  "Width: " puts putu
  " | Height: " puts putu "\n" puts
end

fn main in end
.end
```

**Available parameter**

| Name                              |                                                                                                                                                                                        Usage |
| :-------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------: |
| `deprecated`                      |                                                                                                                                                 Function Only: Mark a function as deprecated |
| `__supports_linux__`              |                                                                                                                                                            Mark assembly as supporting Linux |
| `__supports_skyvm__`              |                                                                                                                                                            Mark assembly as supporting skyvm |
| `__supports_x86_64__`             |                                                                                                                                                           Mark assembly as supporting x86_64 |
| `__supports_x86_64-linux__`       |                                                                                                                                                 Mark assembly as supporting x86_64 for linux |
| `__supports__` <target>           |                                                                                                                                                         Mark assembly as supporting <target> |
| `__typecheck_ignore__`            |                                                                                                                                                                 dont typecheck this function |
| `__provided_externally__` <name?> |                Mark a function as provided by another file. The label name can be specified by setting the parameter to it, otherwise it will use the functions name. It wil call that label |
| `__export__`                      |                                                                                                                               When the parser or vm supports exporting, export this function |
| `__function_exits__`              |                                                                                                        This function will never return. All `ret` statements will get replaced with a panic. |
| `__run_function__`                | This will cause the function to run before main. It has to have an empty in and out. All `__run_function__` marked functions will be ran in an arbitrary sequence, before the main function. |
| `__nomain__`                      |                                                               Apply this to the main function. It will cause it to not run it and instead just exit. May be useful for webassembly-alike vms |
| `__fn_anonymous__`                |                                                               Mark an function as anonymous. This will make it not being assigned to its name. Useful in conjunction with `__run_function__` |
| `__fn_anon__`                     |                                                               Mark an function as anonymous. This will make it not being assigned to its name. Useful in conjunction with `__run_function__` |

|

# Code

```
<keyword-def>
<constant-name>
<memory-name>
<function-name>
<value-integer>
<value-string>
<value-cstring>
<value-charcode>
```

# ConstMemoryCode

```
<constant-name>
<value-integer>
+
*
-
/
%
cast(int)
cast(ptr)
cast(bool)
offset (const only)
reset (const only)
```

# Predefined Constants

UTC means UTC+0

A version looks like this: `major.minor.patchlevel`. For `1.2.5` that means that the major is 1, the minor 2 and the patchlevel 5.

| Name                        |                                                  Value |
| :-------------------------- | -----------------------------------------------------: |
| `__UNIXTIME__`              |                The time at compilation is milliseconds |
| `__DATE_UTC_DAY__`          |                             The UTC Day at compilation |
| `__DATE_UTC_WEEK_DAY__`     |                         The UTC Weekday at compilation |
| `__DATE_UTC_MONTH__`        |                           The UTC Month at compilation |
| `__DATE_UTC_YEAR__`         |                            The UTC Year at compilation |
| `__TIME_UTC_HOURS__`        |                 The UTC Hour at compilation (24 hours) |
| `__TIME_UTC_MINUTES__`      |                          The UTC Minute at compilation |
| `__TIME_UTC_SECONDS__`      |                          The UTC Second at compilation |
| `__TIME_UTC_MILLISECONDS__` |                    The UTC Milliseconds at compilation |
| `---`                       |                                                  `---` |
| `__DATE_DAY__`              |             The Day at compilation (timezone adjusted) |
| `__DATE_WEEK_DAY__`         |         The Weekday at compilation (timezone adjusted) |
| `__DATE_MONTH__`            |           The Month at compilation (timezone adjusted) |
| `__DATE_YEAR__`             |            The Year at compilation (timezone adjusted) |
| `__TIME_HOURS__`            | The Hour at compilation (timezone adjusted) (24 hours) |
| `__TIME_MINUTES__`          |          The Minute at compilation (timezone adjusted) |
| `__TIME_SECONDS__`          |          The Second at compilation (timezone adjusted) |
| `__TIME_MILLISECONDS__`     |    The Milliseconds at compilation (timezone adjusted) |
| `__UNDEFIED_MAJOR__`        |                             The Compiler Version major |
| `__UNDEFIED_MINOR__`        |                             The Compiler Version minor |
| `__UNDEFIED_PATCHLEVEL__`   |                       The Compiler Version patch level |
| `__OPTIMIZATIONS__`         |   The Optimization level it got compiled with (0 or 1) |
| `__TYPECHECK__`             |        Set to true, when run in typechecking only mode |
| `__TARGET__`                |                    Set to the id of the current target |
| `__TARGET_LINUX__`          |                      Set to the id of the linux target |

**Note**

You can add your own predefined constants at compile time by adding arguments that follow this standard:

`-D<constant-name>=<number-above-0>`
`--D<constant-name>=<number-above-0>`

The `D` can be lowercase

> **Usecases**
>
> When you are developing a multi-platform system, the code should know what platform you are on.
>
> You can use -DPLATFORM=0 to set the PLATFORM constant to zero, that could be for example your Linux version.

```rb
const PLATFORM_UNDEFINED 0 end
const PLATFORM_WINDOWS 1 end
const PLATFORM_LINUX 2 end
const PLATFORM_MACOS 3 end

.ifndef PLATFORM const PLATFORM PLATFORM_UNDEFINED end .end
.ifdef PLATFORM .isn const PLATFORM undef PLATFORM const PLATFORM PLATFORM_UNDEFINED end .end .end

.if PLATFORM PLATFORM_UNDEFINED .error "No platform specified" .end
.if PLATFORM PLATFORM_WINDOWS .log "Compiling for windows" .end
.if PLATFORM PLATFORM_LINUX .log "Compiling for Linux" .end
.if PLATFORM PLATFORM_MACOS .log "Compiling for MacOS" .end
```

> **Compilation outputs**

```
$ undefied com main.undefied
Error: main.ts:9:34: No platform specified

$ undefied com main.undefied -DPLATFORM=1
Info: main.ts:9:34: Compiling for Windows
[INFO] [CMD]: Running nasm felf64 -o "/main.o" "/main.asm"
[INFO] [CMD]: Running ld -o "/main" "/main.o"

$ undefied com main.undefied -DPLATFORM=2
Info: main.ts:9:34: Compiling for Linux
[INFO] [CMD]: Running nasm felf64 -o "/main.o" "/main.asm"
[INFO] [CMD]: Running ld -o "/main" "/main.o"

$ undefied com main.undefied -DPLATFORM=3
Info: main.ts:9:34: Compiling for MacOS
[INFO] [CMD]: Running nasm felf64 -o "/main.o" "/main.asm"
[INFO] [CMD]: Running ld -o "/main" "/main.o"

$ undefied com main.undefied -DPLATFORM=4
[INFO] [CMD]: Running nasm felf64 -o "/main.o" "/main.asm"
[INFO] [CMD]: Running ld -o "/main" "/main.o"
```

# Predefined macros

| Name                        |                                                         Value |
| :-------------------------- | ------------------------------------------------------------: |
| `__UNDEFIED_VERSION__`      |           The compiler version during compilation as a string |
| `__UNDEFIED_VERSION_CSTR__` |          The compiler version during compilation as a cstring |
| `__BASE_FILE__`             |  The file that the compilation command was run on as a string |
| `__BASE_FILE_CSTR__`        | The file that the compilation command was run on as a cstring |
