# ASM SYNTAX

Registers:

`%reg` iex. `%rax` or `%flags`

Numbers:

`number` (base-2, base-7, base-10 and base-16 are supported), iex. `0x12`, `1248`, `0o23`, `0b10010011`

Labels (memory, normal)

`.<labelname>`

Anything else is gonna be interpreted as syntax

# Defining a Label:

`#label <labelname>`

# Comments

Anything beginning with ; is a one line comment

# Definining static memory:

Zero Initialised:

syntax: `#memory <label> <size>`

Value Initialised:

syntax: `#value <label> <size> <...values> <newline>`; iex. `#value mystr 0x54 0x68 0x69 0x73 0x20 0x69 0x73 0x20 0x61 0x20 0x73 0x74 0x72 0x69 0x6e 0x67 0x0 ; reserves memory holding "This is a string\x00"`

size values: `b|byte (8 bit)`, `w|word (16 bit)` and `d|dword (32 bit)`

syntax values: `<number>`, iex. `0x24`

# Instructions:

| Name                     |                                                                                           Description |
| :----------------------- | ----------------------------------------------------------------------------------------------------: |
| `mov %rega, %regb`       |                                                                               copies %regb into %rega |
| `mov %reg, num`          |                                                                                  copies num into %reg |
| `push %reg`              |                                                                              Pushes %reg on the stack |
| `push num`               |                                                                               Pushes num on the stack |
| `pop %reg`               |                                                                              Pops %reg from the stack |
| `store8 [num], %reg`     |                                                                Stores %reg to the address num (8 bit) |
| `store8 [%rega], %regb`  |                                                           Stores %regb into the address %rega (8 bit) |
| `load8 [num], %reg`      |                                                                 Loads the address num to %reg (8 bit) |
| `load8 [%rega], %regb`   |                                                            Loads the address %rega into %regb (8 bit) |
| `store16 [num], %reg`    |                                                               Stores %reg to the address num (16 bit) |
| `store16 [%rega], %regb` |                                                          Stores %regb into the address %rega (16 bit) |
| `load16 [num], %reg`     |                                                                Loads the address num to %reg (16 bit) |
| `load16 [%rega], %regb`  |                                                           Loads the address %rega into %regb (16 bit) |
| `store32 [num], %reg`    |                                                               Stores %reg to the address num (32 bit) |
| `store32 [%rega], %regb` |                                                          Stores %regb into the address %rega (32 bit) |
| `load32 [num], %reg`     |                                                                Loads the address num to %reg (32 bit) |
| `load32 [%rega], %regb`  |                                                           Loads the address %rega into %regb (32 bit) |
| `add %reg, num`          |                                                                                      Adds num to %reg |
| `inc %reg`               |                                                                                       increments %reg |
| `sub %rega, %regb`       |                                                                            Subtracts %regb from %rega |
| `sub %reg, num`          |                                                                               Subtracts num from %reg |
| `dec %reg`               |                                                                                       Decrements %reg |
| `mul %rega, %regb`       |                                                                             Multiplies %rega by %regb |
| `mul %reg, num`          |                                                                                Multiplies %reg by num |
| `div %rega, %regb`       | Divides %rega by %regb and puts the remainder in rbx .If %rega is rbx, only the remainder gets stored |
| `div $reg, num`          |     Divides %reg by num and puts the remainder in rbx. If %reg is rbx, only the remainder gets stored |
| `div %rega, %regb`       | Divides %rega by %regb and puts the remainder in rbx. If %rega is rbx, only the remainder gets stored |
| `div $reg, num`          |     Divides %reg by num and puts the remainder in rbx. If %reg is rbx, only the remainder gets stored |
| `div %rega, %regb`       | Divides %rega by %regb and puts the remainder in rbx. If %rega is rbx, only the remainder gets stored |
| `div $reg, num`          |     Divides %reg by num and puts the remainder in rbx. If %reg is rbx, only the remainder gets stored |
| `cmp %rega, %regb`       |                                 Compares %rega and %regb (Does division, but doesnt store the result) |
| `jmp label`              |                                                                                    Jumps to the label |
| `je label`               |                                                                 Jumps to label if the ZeroFlag is set |
| `jne label`              |                                                             Jumps to label if the ZeroFlag is not set |
| `jl label`               |                                    Jumps to label if the ZeroFlag is not set and the CarryFlag is set |
| `jle label`              |                                                                Jumps to label if the CarryFlag is set |
| `jg label`               |                                Jumps to label if the ZeroFlag is not set and the CarryFlag is not set |
| `jge label`              |                                                            Jumps to label if the CarryFlag is not set |
| `call label`             |                                                                     Jumps to the label and pushes rsp |
| `call %reg`              |                                                              Jumps to the address %reg and pushes rsp |
| `ret`                    |                                                                               Pops rsp from the stack |
| `hlt`                    |                                                                                   Halts the Processor |

# Offset

you might want the start to be later than 0x0. For that use the `-offset=<offset>` or `#offset <length>`. Note: all offsets are added together

# Registers

Registers are the following: `rax`, `rbx`, `rcx`, `rdi`, `rsi`, `r10`, `r9`, `r8`, `r7`, `r6`, `r5`, `r4`, `r3`, `rsp`, `flags`, `rip`

# memory protection algorithm (very secure:tm:):

```c
uint32_t make_addr(uint32_t address, uint32_t pid, uint32_t bitmask, uint32_t prefix) {
    if pid == 0 {
        return address;
    }

    return (address & bitmask) | (prefix & ~bitmask);
}
```