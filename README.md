# xComputer Emulator

This repository contains an emulator for David Eck's xComputer and an Assembler which takes relocatable xComputer assembly programs to machine code.

```{.Bash}
stack build
```

produces an executable, which takes the filepath of a program, translates the program to a memory map starting at memory location 0 and then runs the program.
