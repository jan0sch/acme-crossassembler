# Mirror of the ACME crossassembler repository

[![Build Status](https://travis-ci.org/jan0sch/acme-crossassembler.svg?branch=master)](https://travis-ci.org/jan0sch/acme-crossassembler)

The [ACME crossassembler](https://sourceforge.net/projects/acme-crossass/) 
is developed using a subversion repository. This repository is a mirror 
maintained for my own purposes.

It is used to cross compile for example for the Commodore 64.
You'll need it if you want to build the 
[GoDot](https://github.com/godot64/GoDot) project.

## Compilation

Just type `make` or `gmake` (on BSD) from the `src` directory.
The Makefile expects `gcc` to be the GNU CC compiler.

It will produce a binary file `acme` within the `src` folder which you
can copy to a location of your choice.

