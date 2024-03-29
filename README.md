# mary

This is a script compiler for Harvest Moon: Friends of Mineral Town and Harvest Moon: More Friends of Mineral Town for the GBA. The end goal is for it to be able to recompile decompiled vanilla scripts into the exact same bytecode.

This is a very rough and minimal version that I wrote over the weekend. I think it works, but I haven't had the chance to test or document much of it yet.

If you are looking for my old attempt at writing this in C++, see [StanHash/mary_old][mary_old].

[mary_old]: https://github.com/StanHash/mary_old

## Usage

    mary INPUT -o OUTPUT

The input is a file written in a custom script syntax. The output is a fragment of C that defines the data objects for the scripts defined in the source file.

Other options:

    # output to stdout
    mary INPUT

    # output to binary file instead of C (only one script can be defined)
    mary INPUT -o OUTPUT --binary

    # output with a textual representation of the generated intermediate representation (for debugging codegen)
    mary INPUT --debug-ir

## Build

Install Rust development tools if you haven't already. (cargo, rustc, etc.)

    cargo build --release

You will find the mary executable into the target/release directory.

## Example script

    // Script 1 of MFoMT
    // (same as FoMT except for slightly offset native callable ids)

    func 0x106 Func106()
    func 0x117 Func117(a)
    proc 0x105 Proc105(a, b, c)
    proc 0x01F Proc01F()
    proc 0x022 Proc022(a)
    proc 0x021 Proc021()
    proc 0x011 Proc011(a, b, c)

    const CONST_36 = 0x36
    const MESSAGE = "\xFF%is \r\nprengant!\x05"

    script 1 EventScript_1
    {
        var var_0 = Func106()
        var var_1 = CONST_36 + var_0

        if Func117(var_0)
        {
            Proc105(0, 1, var_0)
        }
        else
        {
            Proc105(0, 2, var_0)
        }

        Proc01F()
        Proc022(MESSAGE)
        Proc021()
        Proc011(var_1, 1, 0)
    }

## Script constructs

    // line comment

    /*
    multiline
    comment
    */

### Toplevel constructs

    func ID NAME(PARAM, ...) // defines a native function.
    proc ID NAME(PARAM, ...) // procs are functions that do not return a value.

    const NAME = VALUE // defines a constant. Constants must be evaluatable at compile-time

    script ID NAME { ... } // defines a script

### Script body constructs

    // define a constant, same rules as toplevel constants
    const NAME = VALUE, ...

    // define a variable
    var NAME, ...
    var NAME = VALUE, ...

    // assign to a variable
    NAME = VALUE

    // call a function or procedure
    NAME(ARG, ...)

    // conditional. Braces are mandatory
    if EXPR { ... }
    if EXPR { ... } else { ... }

    // for loop. Braces are mandatory
    for var i = 0; i < 10; i++ { ... }

    // do-while loop. Braces are mandatory
    do { ... } while EXPR

    // switch statement
    switch EXPR
    {
        case VALUE { ... }
        ...
        default { ... } // optional
    }

## TODO

- Unit tests
- Decompiler (matching and symmetrical with compiler)
- Test correctness of generated code
- Better error checking (function/procedure call parameter count coherence)
- Better error reporting (locations...)

## See also

- **[StanHash/FOMT-DOC][FOMT-DOC]**: My docs on FoMT event scripts (and more).
- **[StanHash/fomt][fomt]**: My attempt at/fork of the FoMT decompilation.

[FOMT-DOC]: https://github.com/StanHash/FOMT-DOC
[fomt]: https://github.com/StanHash/fomt
