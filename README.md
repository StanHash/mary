# mary

`mary` is a script compiler and decompiler for Harvest Moon: Friends of Mineral Town and Harvest Moon: More Friends of Mineral Town for the GBA.

It is able to decompile most vanilla scripts and recompile them into the exact same bytecode: as of this writing, 97.59% (1296/1328) or FoMT scripts and 98.44% (1393/1415) of MFoMT scripts can be decompiled, and all of them are exact matches once recompiled. One of the end goals of this project is to reach 100%.

The easiest way to get started would be, I think, to simply decompile a bunch of vanilla scripts to see what they are like, and perhaps modify and recompile them and see what they do in-game.

If you are looking for my old attempt at writing this in C++, see [StanHash/mary_old][mary_old].

[mary_old]: https://github.com/StanHash/mary_old

## Usage

There are two modes of operation when using `mary`: script compilation (convert your own scripts to bytecode), and script decompilation (extract existing bytecode in the ROM as a text)

### Script compilation

    mary compile INPUT -o OUTPUT

The input is a file written in a custom script syntax. The output is a fragment of C that defines the data objects for the scripts defined in the source file.

Other options:

    # output to stdout
    mary compile INPUT

    # output to binary file instead of C (in that mode, only one script can be defined)
    mary compile INPUT -o OUTPUT --binary

    # output with a textual representation of the compiled scripts' intermediate representation (for debugging)
    mary compile INPUT --print-ir

    # compile using a cpp as a preprocessor. (mary can also take input from stdin)
    cpp INPUT | mary compile -o OUTPUT

Run `mary compile --help` for option details.

### Script decompilation

    mary decompile ROM LIBRARY -o OUTPUT --script-id ID

The rom is one of "Harvest Moon - Friends of Mineral Town (USA)" or "Harvest Moon - More Friends of Mineral Town (USA)". Support for other regional variants is not planned yet.

The "library" is a script that defines which functions and procedures are available to scripts. This can by any script, but the decompiler only cares for the function and procedures it defines. Working libraries for FoMT and MFoMT are available in the goodies subdirectory. See details below.

An alternative command syntax allows decompiling from an arbitrary binary rather than one of the two known ROMs:

    mary decompile BINARY LIBRARY -o OUTPUT [--offset OFFSET]

Other options:

    # output to stdout
    mary decompile BINARY LIBRARY

    # output with a textual representation of the decoded scripts' intermediate representation (for debugging)
    mary decompile ROM LIBRARY -o OUTPUT --script-id ID --print-ir

Run `mary decomile --help` for option details.

## Build

Install Rust development tools if you haven't already.

    cargo build --release

You will find the mary executable into the target/release directory.

## About event scripts

In Harvest Moon: (More) Friends of Mineral Town, most interactions and cutscenes are described using event scripts (or simply "scripts"). It is not yet well known how we can configure how scripts are triggered, but with `mary` we can at least modify their actual behavior fairly extensively.

In the ROMs, scripts are represented as bytecode. This bytecode is interpreted by the game engine which provides specific interfaces (procedures and functions) for scripts to interract with the game. This bytecode is not very convenient to read or modify as is, so specific tooling is desired.

A large table of pointers to this bytecode is located within the ROM, one entry per script. "script IDs" are indices into this table. The first entry of this table (script ID of 0) is a null pointer and we ignore it.

Event script pointer table location in each game:

- FoMT: address 0x080F89D4 (offset 0x0F89D4)
- MFoMT: address 0x081014BC (offset 0x1014BC)

There are 1328 scripts in FoMT (IDs 1 through 1328) and 1415 scripts in MFoMT (IDs 1 through 1415).

The script interpreter is a stack-based virtual machine. Usually, stack-based instruction sets map well to higher level constructions, and this is no exception.

## `mary` specific scripting concepts

### Function & Procedure libraries

Internally, the virtual machine only offers the script to invoke native code through IDs. In order for `mary` to be able to compile or decompile scripts, we need to assign a "shape" to each callable ID. A "shape" defines whether a callable is a function and a procedure, as well as what parameters they take.

By writing a program analyzing the bytecode corpus of both games, I have managed to build near-complete callable libraries for both games, which I completed through manual reverse-engineering. Some callables are unreferenced, and therefore couldn't be shaped by my program. (I haven't bothered doing it manually yet either)

These libraries are available as the following files:

- FoMT: [goodies/lib_fomt.txt]
- MFoMT: [goodies/lib_mfomt.txt]

Note that the FoMT uses MFoMT names, which embed MFoMT IDs. This is done for parity between the libraries, but does mean that there's a mismatch between the names used by `mary` scripts and the internal IDs they correspond to. (`Func106` maps to ID 0x106 in MFoMT but ID 0x103 in FoMT)

[goodies/lib_fomt.txt]: goodies/lib_fomt.txt
[goodies/lib_mfomt.txt]: goodies/lib_mfomt.txt

### Typing

Native callable parameters have types. For now those types basically boil down to either being `string` or not. `string`-type parameters are used by the decompiler to know where to place references to string constants.

Non-`string` types can be assigned to parameters as well, but for now the purpose of doing so is at best only informative to the reader. (`mary` itself doesn't care) There may be plans in the future to use these types for producing improved decompilations.

### Constants

Constants don't have any correspondance in the compiled bytecode. They are evaluated at compile time and any reference to a constant is substitued with its value.

## Example script

    // Script 1 of MFoMT
    // (same as FoMT except for slightly offset native callable ids)

    // see goodies/lib_[m]fomt.txt for a full list of known callable shapes
    func 0x106 Func106()
    func 0x117 Func117(idx)
    proc 0x105 Proc105(string_slot, animal_kind, idx)
    proc 0x01F Proc01F()
    proc 0x022 Proc022(message : string)
    proc 0x021 Proc021()
    proc 0x011 Proc011(arg_0, arg_1, arg_1)

    const CONST_36 = 0x36

    script 1 EventScript_1
    {
        const MESSAGE = "\xFF%is \r\nprengant!\x05"

        var animal_id = Func106()
        var var_1 = CONST_36 + animal_id

        if Func117(animal_id)
        {
            // cow
            Proc105(0, 1, animal_id)
        }
        else
        {
            // sheep
            Proc105(0, 2, animal_id)
        }

        Proc01F()
        Proc022(MESSAGE)
        Proc021()
        Proc011(var_1, 1, 0)
    }

## Script syntax

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
        case VALUE, ... { ... }
        ...
        default { ... } // optional
    }

Note that the switch statement syntax may change by 1.0.0, in order to allow constructs that are needed for matching the last few scripts.

## Broken decompiled scripts

| Which game | Broken due to bugged lead assignment | Broken due to incomplete switch decompilation logic |
| ---------- | --- | ---
| FoMT (US) | 106, 143, 409, 410, 411, 532, 879 | 356, 403, 540, 571, 610, 614, 617, 620, 623, 625, 629, 632, 635, 638, 640, 644, 647, 650, 653, 855, 858, 861, 1008, 1013, 1031
| MFoMT (US) | 114, 151, 418, 419, 420, 541, 947 | 359, 365, 412, 433, 549, 580, 726, 1075, 1078, 1083, 1086, 1095, 1098, 1101, 1113

## TODO

- More automated tests
- Better error checking (function/procedure call parameter count coherence)
- Better error reporting (locations...)
- Constant evaluations need to be completed (should be easy)
- Rethink switch decompilation. Most of the failed decompilations are results of weird switch constructs that I don't handle well currently.

## See also

- **[StanHash/FOMT-DOC][FOMT-DOC]**: My docs on FoMT event scripts (and more).
- **[StanHash/fomt][fomt]**: My attempt at/fork of the FoMT decompilation.

[FOMT-DOC]: https://github.com/StanHash/FOMT-DOC
[fomt]: https://github.com/StanHash/fomt
